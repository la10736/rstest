/// A test context.
#[non_exhaustive]
pub struct Context {
    /// The complete module test path  
    pub module: &'static str,
    /// The complete test name  
    pub name: &'static str,
    /// The optional test description
    pub description: Option<&'static str>,
    /// The optional case number
    pub case: Option<usize>,
    /// Start time
    pub start: std::time::Instant,
}

impl Context {
    pub fn new(
        module: &'static str,
        name: &'static str,
        description: Option<&'static str>,
        case: Option<usize>,
    ) -> Self {
        Self {
            module,
            name,
            description,
            case,
            start: std::time::Instant::now(),
        }
    }
}
