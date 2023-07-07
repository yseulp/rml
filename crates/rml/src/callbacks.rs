use rustc_driver::Callbacks;

pub struct ExtractSpec {
    _opts: (),
}

impl ExtractSpec {
    pub fn new(opts: ()) -> Self {
        Self { _opts: opts }
    }
}

impl Callbacks for ExtractSpec {}
