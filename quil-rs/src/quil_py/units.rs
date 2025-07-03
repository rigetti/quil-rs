#[derive(Clone, Copy, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Cycles<T>(pub T);

#[derive(Clone, Copy, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Radians<T>(pub T);

impl From<Cycles<f64>> for Radians<f64> {
    fn from(cycles: Cycles<f64>) -> Self {
        Radians(cycles.0 * 2.0 * std::f64::consts::PI)
    }
}

impl From<Radians<f64>> for Cycles<f64> {
    fn from(radians: Radians<f64>) -> Self {
        Cycles(radians.0 / (2.0 * std::f64::consts::PI))
    }
}
