use std::io;
use std::io::prelude::*;

pub struct StarfishProto<T> { rw: T }

impl<T> StarfishProto<T> {
    pub fn new(rw: T) -> StarfishProto<T> {
        StarfishProto { rw: rw }
    }
}

#[allow(dead_code)]
impl<T: Write> StarfishProto<T> {
    pub fn nop(&mut self) -> io::Result<()> { self.rw.write_all(&[0]) }
    pub fn flush(&mut self) -> io::Result<()> { self.rw.write_all(&[1]) }
    pub fn echo_b(&mut self, b: u8) -> io::Result<()> { self.rw.write_all(&[2, 1, b]) }
    pub fn gpio_in(&mut self, pin: u8) -> io::Result<()> { self.rw.write_all(&[3, pin]) }
    pub fn gpio_high(&mut self, pin: u8) -> io::Result<()> { self.rw.write_all(&[4, pin]) }
    pub fn gpio_low(&mut self, pin: u8) -> io::Result<()> { self.rw.write_all(&[5, pin]) }
    pub fn gpio_toggle(&mut self, pin: u8) -> io::Result<()> { self.rw.write_all(&[21, pin]) }
    pub fn gpio_pull(&mut self, pin: u8) -> io::Result<()> { self.rw.write_all(&[26, pin]) }
    pub fn gpio_cfg(&mut self, pin: u8) -> io::Result<()> { self.rw.write_all(&[6, pin]) }
    pub fn gpio_wait(&mut self, pin: u8) -> io::Result<()> { self.rw.write_all(&[7, pin]) }
    pub fn gpio_int(&mut self, pin: u8) -> io::Result<()> { self.rw.write_all(&[8, pin]) }
    pub fn gpio_input(&mut self, pin: u8) -> io::Result<()> { self.rw.write_all(&[22, pin]) }
    pub fn gpio_raw_read(&mut self, pin: u8) -> io::Result<()> { self.rw.write_all(&[23, pin]) }
    pub fn enable_spi(&mut self, mode: u8, freq: u8, div: u8) -> io::Result<()> { self.rw.write_all(&[10, mode, freq, div]) }
    pub fn disable_spi(&mut self) -> io::Result<()> { self.rw.write_all(&[11]) }
    pub fn enable_i2c(&mut self, freq: u8) -> io::Result<()> { self.rw.write_all(&[12, freq]) }
    pub fn disable_i2c(&mut self) -> io::Result<()> { self.rw.write_all(&[13]) }
    pub fn enable_uart(&mut self, baud: u8, mode: u8) -> io::Result<()> { self.rw.write_all(&[14, baud, mode]) }
    pub fn disable_uart(&mut self) -> io::Result<()> { self.rw.write_all(&[15]) }
    pub fn tx_b(&mut self, b: u8) -> io::Result<()> { self.rw.write_all(&[16, 1, b]) }
    pub fn rx(&mut self, len: u8) -> io::Result<()> { self.rw.write_all(&[17, len, 0x00]) }
    pub fn txrx_b(&mut self, b: u8) -> io::Result<()> { self.rw.write_all(&[18, 1, b]) }
    pub fn start(&mut self, addr: u8) -> io::Result<()> { self.rw.write_all(&[19, addr]) }
    pub fn stop(&mut self) -> io::Result<()> { self.rw.write_all(&[20]) }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Reply {
    Ack,
    Nack,
    High,
    Low,
    Data(u8),
}

impl<T: Read> StarfishProto<T> {
    pub fn recv(&mut self) -> io::Result<Reply> {
        match self.read_byte()? {
            0x80 => Ok(Reply::Ack),
            0x81 => Ok(Reply::Nack),
            0x82 => Ok(Reply::High),
            0x83 => Ok(Reply::Low),
            0x84 => Ok(Reply::Data(self.read_byte()?)),
            _ => Err(io::Error::new(io::ErrorKind::InvalidData, "unexpected byte"))
        }
    }

    fn read_byte(&mut self) -> io::Result<u8> {
        let mut buf = [0u8];
        self.rw.read_exact(&mut buf[..])?;
        debug!("read_byte {}", buf[0]);
        Ok(buf[0])
    }
}
