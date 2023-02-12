# SAMPLE FILES

The files in this directory are raw, headerless audio files for testing samedec.
Play one back with:

```bash
samedec --rate 22050 --file long_message.22050.s16le.bin
```

The filename includes the sampling rate (here `22050` Hz) and sample format
`s16le`. On big endian systems, you must byte-swap the file into big endian
order with something like:

```bash
objcopy \
  -I binary -O binary \
  --reverse-bytes=2 \
  long_message.22050.s16le.bin \
  long_message.22050.s16be.bin
```

You can play one of these files with sox:

```bash
play -t raw -r 22.05k -e signed -b 16 -c 1 'long_message.22050.s16le.bin'
```
