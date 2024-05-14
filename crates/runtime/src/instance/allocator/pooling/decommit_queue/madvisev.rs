const MADVISEV_SYSCALL: libc::c_long = 451;

pub unsafe fn madvisev(memory_ranges: &[libc::iovec], flags: libc::c_int) {
    loop {
        let code = libc::syscall(
            MADVISEV_SYSCALL,
            memory_ranges.as_ptr(),
            memory_ranges.len(),
            flags,
        );
        if code == 0 {
            return;
        }
        let error = std::io::Error::last_os_error();
        if error.raw_os_error() == Some(libc::EAGAIN) {
            continue;
        }
        panic!("madvisev error: {error}");
    }
}
