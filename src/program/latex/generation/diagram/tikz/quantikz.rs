use super::Tikz;

pub struct Quantikz;

impl Tikz for Quantikz {
    fn generate_default_quantikz_diagram(&self) {
        // TODO: Generate a Quantikz diagram of the program with default settings.
        println!("Generating default Quantikz circuit.")
    }    
}