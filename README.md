OhdsiSharing
============

Introduction
============

This is an R package for sharing data between OHDSI partners.

Features
========

- Encrypting and decrypting data using public-private key pairs.

Examples
========

```r
generateKeyPair("public.key", "private.key")
encryptFile("data.rds", "data.rds.enc", "public.key")
decryptFile("data.rds.enc", "data2.rds", "private.key")
```

Technology
============
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
install.packages("devtools")
library("devtools")
install_github("ohdsi/OhdsiSharing")
```

Getting Involved
=============
* Package manual: [OhdsiSharing manual](https://raw.githubusercontent.com/OHDSI/OhdsiSharing/master/man/OhdsiSharing.pdf) 
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
OhdsiSharing is licensed under Apache License 2.0

Development
===========
SqlRender is being developed in R Studio.

###Development status

Under development


