OhdsiSharing
============

[![Build Status](https://travis-ci.org/OHDSI/OhdsiSharing.svg?branch=master)](https://travis-ci.org/OHDSI/OhdsiSharing)

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
Running the package requires R. Also requires Java 1.6 or higher.

Dependencies
============
 * There are no dependencies.

Getting Started
===============

Use these commands in R to download and install the OhdsiSharing package:

```r
install.packages("drat")
drat::addRepo("OHDSI")
install.packages("OhdsiSharing")
```

User Documentation
==================
* Package manual: [OhdsiSharing.pdf](https://raw.githubusercontent.com/OHDSI/OhdsiSharing/master/extras/OhdsiSharing.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/OhdsiSharing/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
OhdsiSharing is licensed under Apache License 2.0. OhdsiSharing uses the [Java Secure Channel (JSch) library](http://www.jcraft.com/jsch/), which is licensed under [BSD syle license](http://www.jcraft.com/jsch/LICENSE.txt).

Development
===========
OhdsiSharing is being developed in R Studio.

### Development status

Under development


