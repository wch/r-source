require("tools")

## -- MD5 test vectors
## RFC 1321 A.5:
stopifnot(identical(md5sum(bytes=raw()),
  "d41d8cd98f00b204e9800998ecf8427e"))
stopifnot(identical(md5sum(bytes=charToRaw("a")),
  "0cc175b9c0f1b6a831c399e269772661"))
stopifnot(identical(md5sum(bytes=charToRaw("abc")),
    "900150983cd24fb0d6963f7d28e17f72"))
## not official, but the FIPS180-2 vectors
stopifnot(identical(md5sum(bytes=rep(charToRaw("a"),1e6)),
  "7707d6ae4e027c70eea2a935c2296f21"))
stopifnot(identical(md5sum(bytes=charToRaw("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")),
  "8215ef0796a20bcaaae116d3876c664a"))

## -- SHA256 test vectors
stopifnot(identical(sha256sum(bytes=raw()),
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))

## FIPS180-2 Appendix B.3 test vectors
stopifnot(identical(sha256sum(bytes=charToRaw("abc")),
  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
stopifnot(identical(sha256sum(bytes=charToRaw("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")),
  "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"))
stopifnot(identical(sha256sum(bytes=rep(charToRaw("a"),1e6)),
  "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"))

## let's see if there are hidden HMAC functions - if so, test them
if (!is.null(environment(tools::sha256sum)[["hmac"]])) { 

## [[ we have not expoorted HMAC functions yet ]]
hmac.md5 <- tools:::hmac.md5
hmac.sha256 <- tools:::hmac.sha256

## -- HMAC test vectors
## RFC 2202 HMAC-MD5
stopifnot(identical(hmac.md5(rep(as.raw(0xb),16), charToRaw("Hi There")),
  "9294727a3638bb1c13f48ef8158bfc9d"))
stopifnot(identical(hmac.md5(charToRaw("Jefe"), charToRaw("what do ya want for nothing?")),
  "750c783e6ab0b503eaa86e310a5db738"))
stopifnot(identical(hmac.md5(rep(as.raw(0xaa),16), rep(as.raw(0xdd), 50)),
  "56be34521d144c88dbb8c733f0e8b3f6"))
## skipping test_case = 4,5
stopifnot(identical(hmac.md5(rep(as.raw(0xaa),80), charToRaw("Test Using Larger Than Block-Size Key - Hash Key First")),
  "6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd"))
stopifnot(identical(hmac.md5(rep(as.raw(0xaa),80), charToRaw("Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data")),
  "6f630fad67cda0ee1fb1f562db3aa53e"))

## RFC 4231 HMAC-SHA256
stopifnot(identical(hmac.sha256(rep(as.raw(0xb),20), charToRaw("Hi There")),
  "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"))
stopifnot(identical(hmac.sha256(charToRaw("Jefe"), charToRaw("what do ya want for nothing?")),
  "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"))
stopifnot(identical(hmac.sha256(rep(as.raw(0xaa),20), rep(as.raw(0xdd), 50)),
  "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe"))
## skipping test_case = 4,5 (we don't truncate - left to the user)
stopifnot(identical(hmac.sha256(rep(as.raw(0xaa),131), charToRaw("Test Using Larger Than Block-Size Key - Hash Key First")),
  "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"))
stopifnot(identical(hmac.sha256(rep(as.raw(0xaa),131),
  charToRaw("This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.")),
  "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2"))

}
