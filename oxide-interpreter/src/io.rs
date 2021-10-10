use std::cell::{RefCell, RefMut};
use std::io::{self, Stdin};
use std::rc::Rc;

type WriteStream = dyn io::Write;
type ReadStream = Stdin;

pub trait StreamProvider {
    fn stream_out(&self) -> RefMut<WriteStream>;
    fn stream_err(&self) -> RefMut<WriteStream>;
    fn stream_in(&self) -> RefMut<ReadStream>;
}

type StdoutStream = Rc<RefCell<WriteStream>>;
type StderrStream = Rc<RefCell<WriteStream>>;
type StdinStream = Rc<RefCell<ReadStream>>;

pub type StdStreams = (
    Option<StdoutStream>,
    Option<StderrStream>,
    Option<StdinStream>,
);

pub struct StdStreamProvider {
    stdout: StdoutStream,
    stderr: StderrStream,
    stdin: StdinStream,
}

impl StdStreamProvider {
    pub fn new(streams: Option<StdStreams>) -> Self {
        let streams = streams.unwrap_or((None, None, None));
        let (stdout, stderr, stdin) = (
            streams
                .0
                .unwrap_or_else(|| Rc::new(RefCell::new(std::io::stdout()))),
            streams
                .1
                .unwrap_or_else(|| Rc::new(RefCell::new(std::io::stderr()))),
            streams
                .2
                .unwrap_or_else(|| Rc::new(RefCell::new(std::io::stdin()))),
        );

        Self {
            stdout,
            stderr,
            stdin,
        }
    }
}

impl StreamProvider for StdStreamProvider {
    fn stream_out(&self) -> RefMut<WriteStream> {
        self.stdout.borrow_mut()
    }

    fn stream_err(&self) -> RefMut<WriteStream> {
        self.stderr.borrow_mut()
    }

    fn stream_in(&self) -> RefMut<ReadStream> {
        self.stdin.borrow_mut()
    }
}
