use crate::Program;
use generation::{LatexFactory, diagram::DiagramFactory};

mod generation;

pub struct Request {
    document: Document,
    settings: Option<Settings>,
}

impl Default for Request {
    fn default() -> Self {
        Self { document: Document::Diagram, settings:  None}
    }
}

pub enum Document {
    Diagram,
    None,
}

pub struct Settings;

// TODO: Move inside of the ConcreteFactory
impl Default for Settings {
    fn default() -> Self {
        Self { }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum LatexGenerationError {
    // TODO: Add variants for each error type using `thiserror` crate to return detailed Result::Err.
    #[error("This is an error on {qubit_index}.")]
    SampleError{qubit_index: u32},
    #[error("Error NoRequest: A request is required to use this feature.")]
    NoRequest,
}

#[cfg(feature = "latex")]
pub trait Latex {
    /// Returns a Result containing a string of LaTeX or a LatexGenerationError.
    ///
    /// # Arguments
    /// * `option` - An Option containing a LaTeX Request.
    /// * `Option::Some` - A valid Request that can be parsed.
    /// * `Option::None` - An invalid Request that throws an error.
    fn to_latex(self, request: Request) -> Result<String, LatexGenerationError>;
}

impl Latex for Program {
    fn to_latex(self, request: Request) -> Result<String, LatexGenerationError> {
        match request.document {
            Document::Diagram => {
                let diagram_factory = Box::new(DiagramFactory {});
                let tikz_diagram_factory = diagram_factory.create_tikz_diagram();
                        
                tikz_diagram_factory.generate_default_quantikz_diagram();
            },
            Document::None => {
                panic!("{}", LatexGenerationError::NoRequest);
            },
        }
        
        // // TODO: Generate the Program LaTeX.
        // let latex = "";

        Ok("".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::{Program, Latex, Request, Document};
    use std::str::FromStr;

    /// Take an instruction and return the LaTeX using the to_latex method.
    pub fn get_quantikz_diagram(instructions: &str) -> String {
        let program = Program::from_str(instructions).expect("program should be returned");
        program.to_latex(Request::default()).expect("Quantikz diagram should generate without error")
    }

    #[test]
    #[should_panic(expected = "")]
    /// Test functionality of to_latex using a request with a None document.
    fn test_to_latex_should_panic_with_document_none() {
        // Create an empty program as a string.
        let program = Program::from_str("").expect("empty program should be created");
        program.to_latex(Request {document: Document::None, settings: None}).expect("function should panic with None document");
    }

    #[test]
    /// Test functionality of to_latex using a default request of Document::Diagram and Settings::None.
    fn test_to_latex_with_default_request() {
        // Create an empty program as a string.
        let program = Program::from_str("").expect("empty program should be created");
        program.to_latex(Request::default()).expect("function should work with default request");
    }

    mod gates {
        use crate::program::latex::tests::get_quantikz_diagram;

        #[test]
        fn test_quantikz_default_diagram_gate_x() {
            insta::assert_snapshot!(get_quantikz_diagram("X 0"));
        }

        #[test]
        fn test_quantikz_default_diagram_gate_y() {
            insta::assert_snapshot!(get_quantikz_diagram("Y 1"));
        }

        #[test]
        fn test_quantikz_default_diagram_gate_controlled() {
            insta::assert_snapshot!(get_quantikz_diagram("CONTROLLED H 3 2"));
        }
    }
}
