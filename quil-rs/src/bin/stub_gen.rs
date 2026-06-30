//! This binary is used to generate Python stub files (type hints) for the `quil` package.
//! For more information on why this exists as a separate binary rather than a build script,
//! see the [`pyo3-stub-gen`][] documentation.
//!
//! [`pyo3-stub-gen`]: https://github.com/Jij-Inc/pyo3-stub-gen

#[cfg(feature = "stubs")]
mod main {
    use std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        fs::File,
        io::{self, BufRead, BufReader, BufWriter, Write as _},
        path::{Path, PathBuf},
    };

    /// A map with the following structure:
    ///
    /// ```
    /// btree_map! {
    ///     "python.module.name": hash_map! {
    ///         "ClassName": (
    ///             vec!["ClassParam1", "ClassParam2", …],
    ///             hash_map! {
    ///                 "method_name": vec!["MethodParam1", "MethodParam2", …],
    ///                 …
    ///             },
    ///         ),
    ///         …
    ///     },
    ///     …
    /// }
    /// ```
    type GenericClasses = BTreeMap<
        &'static str, // Module name
        HashMap<
            &'static str, // Class name
            (
                Vec<&'static str>, // Class parameters
                HashMap<
                    &'static str,      // Method name
                    Vec<&'static str>, // Method parameters
                >,
            ),
        >,
    >;

    /// Try to open a file.  Succeeds with `Some((path, file))` if the file was
    /// openable, suceeds with `None` if the file couldn't be opened because it
    /// doesn't exist, and fails in all other cases.
    fn try_open_file<P: AsRef<Path>>(path: P) -> io::Result<Option<(P, BufReader<File>)>> {
        match File::open(path.as_ref()) {
            Ok(file) => Ok(Some((path, BufReader::new(file)))),
            Err(err) => {
                if err.kind() == std::io::ErrorKind::NotFound {
                    Ok(None)
                } else {
                    Err(err)
                }
            }
        }
    }

    /// Rewrite classes in the specified `.pyi` files to add Python generic parameter lists to the
    /// specified classes.
    fn add_generic_parameters(
        root: PathBuf,
        generic_classes: GenericClasses,
    ) -> pyo3_stub_gen::Result<()> {
        for (module, classes) in generic_classes {
            let module_path = root.join(module.replace('.', "/"));
            let (file_path, file) = match try_open_file(module_path.with_added_extension(".pyi"))? {
                Some(success) => success,
                None => try_open_file(module_path.join("__init__.pyi"))?
                    .unwrap_or_else(|| panic!("no stub file found for {module}")),
            };

            let tempfile_path = module_path.with_added_extension(".pyi.tmp");
            let mut tempfile = BufWriter::new(File::create(&tempfile_path)?);

            let mut classes_not_found: BTreeSet<_> = classes.keys().copied().collect();

            let mut current_class = "<no class>".to_owned();
            let mut current_class_methods = &HashMap::new();
            let mut current_class_methods_not_found = BTreeSet::new();
            let finish_class =
                |current_class: &mut String,
                 current_class_methods_not_found: &mut BTreeSet<&str>| {
                    if !current_class_methods_not_found.is_empty() {
                        panic!(
                            "no type stubs found for methods {}",
                            std::mem::take(current_class_methods_not_found)
                                .into_iter()
                                .map(|method| format!("{module}.{current_class}.{method}"))
                                .collect::<Vec<_>>()
                                .join(", ")
                        );
                    }
                    *current_class = "<no class>".to_owned();
                    current_class_methods_not_found.clear();
                };

            for line in file.lines() {
                let line = line?;
                if line.chars().next().is_some_and(|c| !c.is_whitespace()) {
                    finish_class(&mut current_class, &mut current_class_methods_not_found);
                }

                if let Some((class, (parameters, methods))) = line
                    .strip_prefix("class ")
                    .and_then(|line| line.strip_suffix(":"))
                    .and_then(|class| Some((class, classes.get(class)?)))
                {
                    if !classes_not_found.remove(class) {
                        panic!("multiple type stubs found for class {module}.{class}");
                    }
                    write!(tempfile, "class {class}")?;
                    if !parameters.is_empty() {
                        let mut punct = "[";
                        for parameter in parameters {
                            write!(tempfile, "{punct}{parameter}")?;
                            punct = ", ";
                        }
                        write!(tempfile, "]")?;
                    }
                    writeln!(tempfile, ":")?;
                    current_class = class.to_owned();
                    current_class_methods = methods;
                    current_class_methods_not_found = methods.keys().copied().collect();
                } else if let Some((method, parameters, sig_no_lparen)) = line
                    .strip_prefix("    def ")
                    .and_then(|defn| defn.split_once("("))
                    .and_then(|(method, sig_no_lparen)| {
                        Some((method, current_class_methods.get(method)?, sig_no_lparen))
                    })
                {
                    // It's okay to have multiple type stubs for a method – that's what @overload is
                    // for.
                    let _ = current_class_methods_not_found.remove(method);
                    write!(tempfile, "    def {method}")?;
                    if !parameters.is_empty() {
                        let mut punct = "[";
                        for parameter in parameters {
                            write!(tempfile, "{punct}{parameter}")?;
                            punct = ", ";
                        }
                        write!(tempfile, "]")?;
                    }
                    writeln!(tempfile, "({sig_no_lparen}")?;
                } else {
                    writeln!(tempfile, "{line}")?;
                }
            }

            finish_class(&mut current_class, &mut current_class_methods_not_found);

            tempfile.flush()?;
            drop(tempfile);

            if !classes_not_found.is_empty() {
                panic!(
                    "no type stubs found for classes {}",
                    classes_not_found
                        .into_iter()
                        .map(|class| format!("{module}.{class}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }

            std::fs::rename(tempfile_path, file_path)?;
        }

        Ok(())
    }

    /// The classes that need generic parameters added
    fn generic_classes() -> GenericClasses {
        let biparameterized = |name: &'static str| {
            (
                name,
                (
                    vec!["Real", "Complex = Real"],
                    HashMap::from([("evaluate", vec!["OtherReal", "OtherComplex = OtherReal"])]),
                ),
            )
        };

        BTreeMap::from([(
            "quil.waveform",
            HashMap::from([
                biparameterized("Waveform"),
                biparameterized("BuiltinWaveform"),
                biparameterized("CommonBuiltinParameters"),
                biparameterized("Flat"),
                biparameterized("Gaussian"),
                biparameterized("DragGaussian"),
                biparameterized("ErfSquare"),
                biparameterized("HermiteGaussian"),
                (
                    "BoxcarKernel",
                    (
                        vec![],
                        HashMap::from([("iq_values_at_sample_rate", vec!["_T"])]),
                    ),
                ),
            ]),
        )])
    }

    pub fn main() -> pyo3_stub_gen::Result<()> {
        let mut stub = quil_rs::quilpy::stub_info()?;
        rigetti_pyo3::stubs::sort(&mut stub);
        stub.generate()?;
        add_generic_parameters(stub.python_root, generic_classes())?;
        Ok(())
    }
}

#[cfg(not(feature = "stubs"))]
mod main {
    use std::process::ExitCode;

    fn main() -> ExitCode {
        eprintln!("Executing this binary only makes sense with the --stubs feature enabled.");
        ExitCode::FAILURE
    }
}

pub use main::main;
