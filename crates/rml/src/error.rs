use rustc_errors::DiagnosticId;
use rustc_session::Session;
use rustc_span::{Span, DUMMY_SP};

#[derive(Debug)]
pub struct Error {
    span: Span,
    msg: String,
}

impl Error {
    pub(crate) fn new(span: Span, msg: impl Into<String>) -> Self {
        Error {
            span,
            msg: msg.into(),
        }
    }

    pub(crate) fn emit(self, sess: &Session) -> ! {
        sess.span_fatal_with_code(
            self.span,
            self.msg,
            DiagnosticId::Error(String::from("rml")),
        )
    }
}

#[derive(Debug, Clone)]
pub struct RmlErr;

impl From<RmlErr> for Error {
    fn from(_: RmlErr) -> Error {
        Error::new(DUMMY_SP, "internal error")
    }
}

impl std::fmt::Display for RmlErr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "encountered errors during validation")
    }
}
impl std::error::Error for RmlErr {}
