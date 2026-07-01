//! This binary is used to generate Python stub files (type hints) for the `quil` package.
//! For more information on why this exists as a separate binary rather than a build script,
//! see the [`pyo3-stub-gen`][] documentation.
//!
//! [`pyo3-stub-gen`]: https://github.com/Jij-Inc/pyo3-stub-gen

#[cfg(not(feature = "stubs"))]
mod main {
    use std::process::ExitCode;

    pub fn main() -> ExitCode {
        eprintln!("Executing this binary only makes sense with the --stubs feature enabled.");
        ExitCode::FAILURE
    }
}
#[cfg(feature = "stubs")]
/// Our stub generation code generates the `.pyi` files with
/// [`py-stub-gen`](https://github.com/Jij-Inc/pyo3-stub-gen), and then applies four categories of
/// edits to the generated `.pyi` files in order to work around the limitations of
/// [PyO3](https://pyo3.rs/) and `py-stub-gen` when it comes to generic Python types.
///
/// We allow specifying four kinds of edits.  Each edit operates on a single line, but they are
/// shown on multiple lines here for clarity.
///
/// 1. Classes may be altered to have generic parameters.  For instance, this can replace `class
///    Waveform:` with `class Waveform[Real, Complex]:`.
///
/// 2. Methods may be altered to have generic parameters.  For instance, this can replace
///
///    ```text
///    def evaluate(
///        self,
///        real: collections.abc.Callable[[Real], OtherReal],
///        complex: collections.abc.Callable[[Complex], OtherComplex]
///    ) -> Waveform[OtherReal, OtherComplex]:
///        ...
///    ```
///    
///    with
///    
///    ```text
///    def evaluate[OtherReal, OtherComplex = OtherReal](
///        self,
///        real: collections.abc.Callable[[Real], OtherReal],
///        complex: collections.abc.Callable[[Complex], OtherComplex]
///    ) -> Waveform[OtherReal, OtherComplex]:
///    ```
///
/// 3. Methods may have a different type annotation placed on `self`.  For instance, this can
///    replace
///
///    ```text
///    def iq_values_at_sample_rate[_T](
///        self,
///        common: CommonBuiltinParameters[builtins.float, _T],
///        sample_rate: builtins.float
///    ) -> IqSamples:
///        ...
///    ```
///    
///    with
///    
///    ```text
///    def iq_values_at_sample_rate[_T](
///        self: BuiltinWaveform[builtins.float, builtins.complex],
///        common: CommonBuiltinParameters[builtins.float, _T],
///        sample_rate: builtins.float
///    ) -> IqSamples:
///        ...
///    ```
///
///    (Note that this function has already had a generic parameter added by the second edit.)
///
/// 4. Anywhere `$SELF` occurs in a class that is being edited, it is replaced with the
///    *fully-parameterized* form of the class.  This is important for code generation from macros.
///    For instance, this can replace
///
///    ```text
///    def __new__(
///        cls,
///        *,
///        duration: builtins.float,
///        scale: typing.Optional[Real] = None,
///        phase: typing.Optional[Real] = None,
///        detuning: typing.Optional[Real] = None
///    ) -> $SELF:
///        ...
///    ```
///
///    with
///
///    ```text
///    def __new__(
///        cls,
///        *,
///        duration: builtins.float,
///        scale: typing.Optional[Real] = None,
///        phase: typing.Optional[Real] = None,
///        detuning: typing.Optional[Real] = None
///    ) -> CommonBuiltinParameters[Real, Complex]:
///        ...
///    ```
mod main {
    use indexmap::IndexMap;

    mod edits {
        use std::fmt;

        use indexmap::IndexMap;

        pub type Modules = IndexMap<String, IndexMap<String, Class>>;

        #[derive(Clone, Debug)]
        pub struct Class {
            pub type_parameters: Vec<TypeParameter>,
            pub methods: IndexMap<String, Method>,
        }

        #[derive(Clone, Debug)]
        pub struct Method {
            pub type_parameters: Vec<TypeParameter>,
            pub self_type: Option<String>,
        }

        #[derive(Clone, Debug)]
        pub struct TypeParameter {
            name: String,
            default: Option<String>,
        }

        impl fmt::Display for TypeParameter {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let Self { name, default } = self;
                write!(f, "{name}")?;
                if let Some(default) = default {
                    write!(f, " = {default}")?;
                }
                Ok(())
            }
        }

        impl TypeParameter {
            pub fn new<S: Into<String>>(name: S) -> Self {
                Self {
                    name: name.into(),
                    default: None,
                }
            }

            pub fn defaulted<S1: Into<String>, S2: Into<String>>(name: S1, default: S2) -> Self {
                Self {
                    name: name.into(),
                    default: Some(default.into()),
                }
            }
        }

        pub fn fmt_type_parameters_maybe_default(
            parameters: &[TypeParameter],
            defaults: bool,
        ) -> impl fmt::Display + use<'_> {
            fmt::from_fn(move |f| {
                if parameters.is_empty() {
                    return Ok(());
                }
                write!(f, "[")?;
                let mut first = true;
                for p in parameters {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    if defaults {
                        write!(f, "{p}")?;
                    } else {
                        write!(f, "{}", p.name)?;
                    }
                }
                write!(f, "]")
            })
        }

        pub fn fmt_type_parameters(parameters: &[TypeParameter]) -> impl fmt::Display + use<'_> {
            fmt_type_parameters_maybe_default(parameters, true)
        }

        pub fn fmt_type_parameters_defaultless(
            parameters: &[TypeParameter],
        ) -> impl fmt::Display + use<'_> {
            fmt_type_parameters_maybe_default(parameters, false)
        }
    }

    mod editor {
        use std::{
            fs::File,
            io::{self, BufRead, BufReader, BufWriter, Write as _},
            path::{Path, PathBuf},
        };

        use anyhow::bail;
        use indexmap::{IndexMap, IndexSet};

        use super::edits;

        /// Try to open a file.  Succeeds with `Some((path, file))` if the file was openable, suceeds
        /// with `None` if the file couldn't be opened because it doesn't exist, and fails in all other
        /// cases.
        fn try_open_file<P: AsRef<Path>>(path: P) -> io::Result<Option<(P, BufReader<File>)>> {
            match File::open(path.as_ref()) {
                Ok(file) => Ok(Some((path, BufReader::new(file)))),
                Err(err) => {
                    if err.kind() == io::ErrorKind::NotFound {
                        Ok(None)
                    } else {
                        Err(err)
                    }
                }
            }
        }

        /// Given a path to the `root` where the Python `.pyi` files are stored and a Python `module`
        /// name, attempt to open the `.pyi` file for that module.
        fn open_python_module(
            root: &Path,
            module: &str,
        ) -> anyhow::Result<(PathBuf, BufReader<File>)> {
            let module_path = root.join(module.replace('.', "/"));

            if let Some(success) = try_open_file(module_path.with_added_extension(".pyi"))? {
                return Ok(success);
            }

            if let Some(success) = try_open_file(module_path.join("__init__.pyi"))? {
                return Ok(success);
            }

            bail!("no stub file found for {module}")
        }

        /// An editor for adjusting `.pyi` files.
        #[derive(Debug)]
        struct PyiEditor<'a> {
            context: PyiContext<'a>,
            state: PyiEditorState<'a>,
            input: FileInput,
            output: FileOutput,
        }

        /// Global, unchanging information about a `.pyi` file.
        #[derive(Debug)]
        struct PyiContext<'a> {
            module_name: &'a str,
            classes: &'a IndexMap<String, edits::Class>,
        }

        /// Information about the current class a [`PyiEditor`] is looking at, as it goes through the
        /// file line by line.
        #[derive(Debug)]
        struct PyiCurrentClass<'a> {
            class_name: &'a str,
            class: &'a edits::Class,
            unseen_methods: IndexSet<&'a str>,
        }

        /// The updatable state of the editor.
        #[derive(Debug)]
        struct PyiEditorState<'a> {
            unseen_classes: IndexSet<&'a str>,
            current_class: Option<PyiCurrentClass<'a>>,
        }

        /// Information about an input file.
        #[derive(Debug)]
        struct FileInput {
            input_path: PathBuf,
            input: BufReader<File>,
        }

        /// Information about an output file.
        #[derive(Debug)]
        struct FileOutput {
            output_path: PathBuf,
            output: BufWriter<File>,
        }

        impl<'a> PyiEditor<'a> {
            /// Given a path to the `root` where the Python `.pyi` files are stored, a Python
            /// `module` name, and a map of edits to be made to `classes` in that module, construct
            /// the editor that will perform those edits.
            fn for_module(
                root: &Path,
                module_name: &'a str,
                classes: &'a IndexMap<String, edits::Class>,
            ) -> anyhow::Result<Self> {
                let (input_path, input) = open_python_module(root, module_name)?;

                let tempfile_path = input_path.with_added_extension("tmp");
                let tempfile = BufWriter::new(File::create(&tempfile_path)?);

                Ok(Self {
                    context: PyiContext {
                        module_name,
                        classes,
                    },
                    state: PyiEditorState {
                        unseen_classes: classes.keys().map(String::as_str).collect(),
                        current_class: None,
                    },
                    input: FileInput { input_path, input },
                    output: FileOutput {
                        output_path: tempfile_path,
                        output: tempfile,
                    },
                })
            }
        }

        impl<'a> PyiEditorState<'a> {
            /// Once we've gotten to the end of a class, make sure that we've seen all the methods
            /// we're expecting and then clear the class state.
            fn finish_class(&mut self, context: &PyiContext<'a>) -> anyhow::Result<()> {
                let module_name = context.module_name;

                match self.current_class.take() {
                    Some(PyiCurrentClass {
                        class_name,
                        unseen_methods,
                        class: _,
                    }) if !unseen_methods.is_empty() => {
                        bail!(
                            "no type stubs found for methods: {}",
                            unseen_methods
                                .into_iter()
                                .map(|method| format!("{module_name}.{class_name}.{method}"))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    }

                    Some(PyiCurrentClass { .. }) | None => Ok(()),
                }
            }

            /// Given a line, print the resulting possibly-edited line and update this state.
            fn update_and_output<W: io::Write>(
                &mut self,
                context: &PyiContext<'a>,
                mut output: W,
                line: &str,
            ) -> anyhow::Result<()> {
                let PyiContext {
                    module_name,
                    classes,
                } = context;

                if line.chars().next().is_some_and(|c| !c.is_whitespace()) {
                    self.finish_class(context)?;
                }

                let Self {
                    unseen_classes,
                    current_class,
                } = self;

                if let Some((class_name, class)) = line
                    .strip_prefix("class ")
                    .and_then(|classless| classless.strip_suffix(":"))
                    .and_then(|class_name| classes.get_key_value(class_name))
                {
                    if !unseen_classes.shift_remove(class_name.as_str()) {
                        bail!("duplicate occurrences of class {module_name}.{class_name}");
                    }

                    *current_class = Some(PyiCurrentClass {
                        class_name,
                        class,
                        unseen_methods: class.methods.keys().map(String::as_str).collect(),
                    });

                    writeln!(
                        output,
                        "class {class_name}{}:",
                        edits::fmt_type_parameters(&class.type_parameters)
                    )?;
                } else if let Some(PyiCurrentClass {
                    class_name,
                    class,
                    unseen_methods,
                }) = current_class.as_mut()
                {
                    let writeln_replacing_self = |output: &mut W, text: &str| {
                        let mut first = true;
                        for fragment in text.split("$SELF") {
                            if first {
                                first = false;
                            } else {
                                write!(
                                    output,
                                    "{class_name}{}",
                                    edits::fmt_type_parameters_defaultless(&class.type_parameters)
                                )?;
                            }
                            write!(output, "{fragment}")?;
                        }
                        writeln!(output)
                    };

                    if let Some((method_name, method, sig_no_lparen)) = line
                        .strip_prefix("    def ")
                        .and_then(|defless| defless.split_once("("))
                        .and_then(|(method_name, sig_no_lparen)| {
                            let method = class.methods.get(method_name)?;
                            Some((method_name, method, sig_no_lparen))
                        })
                    {
                        let edits::Method {
                            type_parameters,
                            self_type,
                        } = method;

                        write!(
                            output,
                            "    def {method_name}{}(",
                            edits::fmt_type_parameters(type_parameters)
                        )?;

                        // Duplicate methods are fine – that's what `@overload` is
                        unseen_methods.shift_remove(method_name);

                        match self_type {
                            Some(self_type) => match sig_no_lparen.strip_prefix("self,") {
                                Some(selfless) => {
                                    write!(output, "self: {self_type},")?;
                                    writeln_replacing_self(&mut output, selfless)?;
                                }
                                None => bail!(
                                    "no self parameter for method \
                                     {module_name}.{class_name}.{method_name}"
                                ),
                            },
                            None => writeln_replacing_self(&mut output, sig_no_lparen)?,
                        }
                    } else {
                        writeln_replacing_self(&mut output, line)?;
                    }
                } else {
                    writeln!(output, "{line}")?;
                }

                Ok(())
            }
        }

        /// Given a path to the `root` where the Python `.pyi` files are stored, a Python `module`
        /// name, and a map of edits to be made to `classes` in that module, update the `.pyi` file
        /// for the given module.
        pub fn edit_module<'a>(
            root: &Path,
            module_name: &'a str,
            classes: &'a IndexMap<String, edits::Class>,
        ) -> anyhow::Result<()> {
            let PyiEditor {
                context,
                mut state,
                input: FileInput { input_path, input },
                output:
                    FileOutput {
                        output_path,
                        mut output,
                    },
            } = PyiEditor::for_module(root, module_name, classes)?;

            for line in input.lines() {
                state.update_and_output(&context, &mut output, &line?)?;
            }

            output.flush()?;
            drop(output);

            state.finish_class(&context)?;

            if !state.unseen_classes.is_empty() {
                let module_name = context.module_name;
                bail!(
                    "no type stubs found for classes: {}",
                    state
                        .unseen_classes
                        .into_iter()
                        .map(|class_name| format!("{module_name}.{class_name}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }

            std::fs::rename(output_path, input_path)?;

            Ok(())
        }
    }

    /// The classes that need generic parameters added
    fn pyi_edits() -> edits::Modules {
        use edits::{Class, Method, Modules, TypeParameter};

        let real = TypeParameter::new("Real");
        let complex = TypeParameter::defaulted("Complex", "Real");

        let evaluable_with_extras =
            |class_name: &'static str, mut methods: IndexMap<String, Method>| {
                let None = methods.insert(
                    "evaluate".to_owned(),
                    Method {
                        type_parameters: vec![
                            TypeParameter::new("OtherReal"),
                            TypeParameter::defaulted("OtherComplex", "OtherReal"),
                        ],
                        self_type: None,
                    },
                ) else {
                    panic!("tried to request multiple edits for {class_name}.evaluate");
                };

                (
                    class_name.to_owned(),
                    Class {
                        type_parameters: vec![real.clone(), complex.clone()],
                        methods,
                    },
                )
            };

        let builtin_waveform = |name| {
            evaluable_with_extras(
                name,
                IndexMap::from([(
                    "iq_values_at_sample_rate".to_owned(),
                    Method {
                        type_parameters: vec![TypeParameter::new("_T")],
                        self_type: Some(format!("{name}[builtins.float, builtins.complex]")),
                    },
                )]),
            )
        };

        Modules::from([(
            "quil.waveform".to_owned(),
            IndexMap::from([
                evaluable_with_extras(
                    "CommonBuiltinParameters",
                    IndexMap::from([(
                        "resolve_with_sample_rate".to_owned(),
                        Method {
                            type_parameters: vec![TypeParameter::new("_T")],
                            self_type: Some(
                                "CommonBuiltinParameters[builtins.float, _T]".to_owned(),
                            ),
                        },
                    )]),
                ),
                evaluable_with_extras("Waveform", IndexMap::new()),
                builtin_waveform("BuiltinWaveform"),
                builtin_waveform("Flat"),
                builtin_waveform("Gaussian"),
                builtin_waveform("DragGaussian"),
                builtin_waveform("ErfSquare"),
                builtin_waveform("HermiteGaussian"),
                (
                    "BoxcarKernel".to_owned(),
                    Class {
                        type_parameters: vec![],
                        methods: IndexMap::from([(
                            "iq_values_at_sample_rate".to_owned(),
                            Method {
                                type_parameters: vec![TypeParameter::new("_T")],
                                self_type: None,
                            },
                        )]),
                    },
                ),
            ]),
        )])
    }

    pub fn main() -> anyhow::Result<()> {
        let mut stub = quil_rs::quilpy::stub_info()?;
        rigetti_pyo3::stubs::sort(&mut stub);
        stub.generate()?;
        for (module, classes) in pyi_edits() {
            editor::edit_module(&stub.python_root, &module, &classes)?;
        }
        Ok(())
    }
}

pub use main::main;
