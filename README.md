OhdsiSharing
============

[![Build Status](https://github.com/OHDSI/OhdsiSharing/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/OhdsiSharing/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/OhdsiSharing/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/OhdsiSharing?branch=master)

OhdsiSharing is part of [HADES](https://ohdsi.github.io/Hades).

Introduction
============

This is an R package for sharing data between OHDSI partners.

Features
========

- Encrypting and decrypting data using public-private key pairs.
- Uploading and downloading files to and from the OHDSI SFTP server.

Examples
========

```r
generateKeyPair("public.key", "private.key")
encryptFile("data.rds", "data.rds.enc", "public.key")
decryptFile("data.rds.enc", "data2.rds", "private.key")
```

Technology
==========
The OhdsiSharing package is an R package. Cryptography uses the Java Cryptography Architecture.

System Requirements
===================
Running the package requires R. Also requires Java 1.8 or higher.

Getting Started
===============

1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including Java.

2. Use these commands in R to download and install the OhdsiSharing package:

    ```r
    install.packages("drat")
    drat::addRepo("OHDSI")
    install.packages("OhdsiSharing")
    ```

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/OhdsiSharing).

PDF versions of the documentation are also available:
* Package manual: [OhdsiSharing.pdf](https://raw.githubusercontent.com/OHDSI/OhdsiSharing/master/extras/OhdsiSharing.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/OhdsiSharing/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

License
=======
OhdsiSharing is licensed under Apache License 2.0. OhdsiSharing uses the [Java Secure Channel (JSch) library](http://www.jcraft.com/jsch/), which is licensed under [BSD style license](http://www.jcraft.com/jsch/LICENSE.txt).

Development
===========
OhdsiSharing is being developed in R Studio.

### Development status

Under development
