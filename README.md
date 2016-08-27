# M2PA Testtool and Testsuite
A test tool for M2PA as specified in [RFC 4165](https://tools.ietf.org/html/rfc4165).
The tests are based on the ETSI test specification
[ETSI TS 101 591](http://www.etsi.org/deliver/etsi_ts/101500_101599/101591/01.01.01_60/ts_101591v010101p.pdf).

## Requirements
This tool uses [guile](https://www.gnu.org/software/guile/) and its extension [guile-sctp](https://github.com/nplab/guile-sctp) for SCTP.

## Supported Platforms
It runs on Unix operating systems providing kernel SCTP support:
* FreeBSD.
* Linux, using the `libsctp-dev`package.
* Mac OS X, using the [SCTP-NKE](https://github.com/sctplab/SCTP_NKE_ElCapitan) for adding kernel SCTP support.
* Solaris.
