use std::sync::atomic::{AtomicBool, Ordering};

pub static VERBOSE_LOGGING: AtomicBool = AtomicBool::new(false);
pub static DEBUG_LOGGING: AtomicBool = AtomicBool::new(false);

pub fn _debug_log<F>(get_s: F)
where
    F: FnOnce() -> String,
{
    if DEBUG_LOGGING.load(Ordering::Relaxed) {
        eprint!("{}\n", get_s());
    }
}

pub fn _verbose_log<F>(get_s: F)
where
    F: FnOnce() -> String,
{
    if VERBOSE_LOGGING.load(Ordering::Relaxed) {
        eprint!("{}\n", get_s());
    }
}

#[macro_export]
macro_rules! verbose_log {
    () => ($crate::puzzlescript::logging::_verbose_log(|| "".to_string()));
    ($($arg:tt)*) => ({
        $crate::puzzlescript::logging::_verbose_log(|| format!($($arg)*));
    })
}

#[macro_export]
macro_rules! debug_log {
    () => ($crate::puzzlescript::logging::_debug_log(|| "".to_string()));
    ($($arg:tt)*) => ({
        $crate::puzzlescript::logging::_debug_log(|| format!($($arg)*));
    })
}
