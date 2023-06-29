use rustc_driver::Callbacks;

pub struct ExtractSpec {
    opts: (),
}

impl ExtractSpec {
    pub fn new(opts: ()) -> Self {
        Self { opts }
    }
}

impl Callbacks for ExtractSpec {}
