use libusb;
use std::io;
use std::io::prelude::*;
use std::time::Duration;
use std::cmp::min;

pub fn find_device<'a>(context: &'a libusb::Context, vid: u16, pid: u16) -> Result<libusb::DeviceHandle<'a>, libusb::Error> {
    let devices = try!(context.devices());
    devices.iter().find(|device| {
        device.device_descriptor().ok().map_or(false, |desc| desc.vendor_id() == vid && desc.product_id() == pid)
    }).map_or(Err(libusb::Error::NotFound), |device| device.open())
}

pub struct StarfishUsb<'c> {
    handle: libusb::DeviceHandle<'c>,
    read_buf: [u8; 64],
    read_size: usize,
    read_pos: usize,
}

impl<'c> StarfishUsb<'c> {
    pub fn new(mut handle: libusb::DeviceHandle) -> Result<StarfishUsb, libusb::Error> {
        try!(handle.claim_interface(0));
        try!(handle.set_alternate_setting(0, 1));
        Ok(StarfishUsb {
            handle: handle,
            read_buf: [0; 64],
            read_size: 0,
            read_pos: 0,
        })
    }
}

fn libusb_err_to_io(e: libusb::Error) -> io::Error {
    let kind = match e {
        libusb::Error::Timeout => io::ErrorKind::TimedOut,
        libusb::Error::NotFound => io::ErrorKind::NotConnected,
        libusb::Error::Interrupted => io::ErrorKind::Interrupted,
        _ => io::ErrorKind::BrokenPipe,
    };
    io::Error::new(kind, e)
}

impl<'c> Read for StarfishUsb<'c> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if self.read_pos >= self.read_size {
            let count = try!(self.handle.read_bulk(0x81, &mut self.read_buf, Duration::from_secs(10)).map_err(libusb_err_to_io));
            self.read_pos = 0;
            self.read_size = count;
        }

        let len = min(self.read_size - self.read_pos, buf.len());
        buf[0..len].copy_from_slice(&self.read_buf[self.read_pos..self.read_pos + len]);
        self.read_pos += len;
        Ok(len)
    }
}

impl <'c> Write for StarfishUsb<'c> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.handle.write_bulk(0x02, buf, Duration::from_secs(10)).map_err(libusb_err_to_io)
    }

    fn flush(&mut self) -> io::Result<()> { Ok(()) }
}
