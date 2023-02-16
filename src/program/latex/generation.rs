use self::diagram::tikz::Tikz;

pub mod diagram;

pub trait LatexFactory {
    fn create_tikz_diagram(&self) -> Box<dyn Tikz>;
}