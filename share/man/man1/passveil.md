---
title: PASSVEIL(1) ANTEI.DE | passveil - distributed password manager
---

NAME
====

passveil - manages and shares passwords as well as trust

SYNOPSIS
========

**passveil** [**\-\-store** DIRECTORY] COMMAND

DESCRIPTION
===========

passveil(1) is a distributed password manager this is using gpg2(1) for
encryption, decryption, signing and verification, as well as darcs(1) for
synchronization.

passveil(1) works like a key value store, we will refer to keys as paths
throughout this documentation to avoid confusion with gpg2(1) keys. Paths are
hashes using SHA256 since passveil(1) does not store them in plain text. This
way it allows to store paths that might reveal sensitive information (like
undisclosed project names) in a shared repository. Paths can be used to
hierarchically structure the store like a file system using '/' as separators.

gpg(2) public and private keys used for everything regarding encryption.
passveil(1) uses key fingerprints to uniquely identify a key.

passveil(1) also manages a notion of "trust". Everyone who has access to a
password can allow (or deny) access that password by adding their gpg(2) key to
a list of trusted keys. Since everyone who is able to decrypt a password should
be able to securely give this data to other trusted people, avoiding insecure
channels (e.g. E-Mail or insecure messengers).

An _index_ of meta data is retained to speed up certain operations. This helps
the reduce needed decryption operations which result in interaction with
gpg2(1). Meta data stored in the _index_ is used for display and search
operations. No secret information is stored inside of it.

Password generation
-------------------

passveil(1) can automatically generate passwords according to a set of rules.
The general form of these rules is:

```
LENGTH[:DEFAULT{:REQUIREMENT}]]
```

Where _LENGTH_ is the length of the password and _DEFAULT_ the default alphabet
that is used to fill up the password. A _REQUIREMENT_ states how often it is
required for a character from an alphabet to appear in a password. For example
if the resulting password has to contain at least one digit the requirement for
this would be _:1d_.

As a more complete example the rule to create a 16 character password with at
least one digit and one upper-case letter while filling up the rest with
lower-case characters would be _16:l:1d:1u_.

An alphabet is made out of one or more character classes. To create an alphabet
out of the character class of digits **d** and the character class of upper-case
characters **u** one can concatenate them into a new alphabet **du**.

The following character classes are available:

* **d** Digits: _\<0-9>_
* **u** Uppercase ASCII letters: _\<A-Z>_
* **l** Lowercase ASCII letters: _\<a-z>_
* **p** ASCII Punctuation: _!"#%&'()\*,-./:;?@\[\\]\_{}_
* **s** ASCII Symbols: _$+<=>^\`|~_

Rules can be validated by using the following EBNF:

```
nonzero = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
number = nonzero, { digit } ;

class = "d" | "u" | "l" | "p" | "s" ;
alphabet = class, { class } ;

requirement = ":",  number , alphabet ;

rules = number, [ ":" , ( alphabet  | "" ) , { requirement } ] ;
```

Trust
-----

Trust can be established between all known gpg2(1) keys. passveil(1) allows
everyone with the knowledge of a password to **allow** and **deny** access to a
password. These operations are logged to helps users to keep track of
permission. This helps to identify passwords that might potentially have been
compromised of a private key accidentally leaked. passveil(1) tries to make
sharing passwords among users as easy as possible to minimize the need to fall
back to insecure communication channels.

Nothing prevents someone with the knowledge of a password to write it down on a
piece of paper and put it on a co-workers desk, therefore using passveil(1) to
do this needs to be simpler than that which is why knowledge can be managed
without the need for a central administrator (who cannot prevent people from
passing around pieces of paper anyway).

OPTIONS
=======

**-h**, **\-\-help**

  Print help for passveil(1).

**\-\-version**

  Show version and exit.

**\-\-store** DIRECTORY

  Specify alternate store.

COMMANDS
========

KEY MANAGEMENT
--------------

Commands that are used to insert, delete, move and edit password data within a
store. The operations need access to the the private gpg2(1) key and therefore
require comunication with gpg2(1).

_**insert** \[\-\-generate RULES] \[\-\-batch] PATH_
-----------------------------------------------------

Inserts a new password into the store. passveil(1) starts *\$EDITOR* as input
method for the new password unless the **\-\-batch** switch has been specified.
passveil(1) will refuse to insert passwords at paths that are already in
existence. For each trusted gpg2(1) key passveil(1) will create a separate file
(using the key fingerprint as file name) that can be optionally signed be the
issuer of the **insert** command to verify the issuers existence.

**\-\-generate** RULES

Automatically generate a password according to a set of rules.

**\-\-batch**

Take input from *stdin* instead of using *\$EDITOR*. Mainly used for scripting.
If **\-\-generate** is specified passveil(1) will insert a randomly generated
password according to the stated rules without running *\$EDITOR*.

**-h**, **\-\-help**

Print help for **insert**.

_**delete** PATH_
-----------------

Delete a password from the store. This command removes the password from the
store entirely, including for everyone in the list of _trusted_ gpg2(1) keys.

**-h**, **\-\-help**

Print help for **delete**.

_**move** PATH PATH_
--------------------

Move a password to another location within the store. Overwriting existing
passwords via this command is not permitted. This command does not change the
creation date of the original password but updates the _issued_ information.

**-h**, **\-\-help**

Print help for **move**.

_**copy** PATH PATH_
--------------------

Copy a password to another location within the store. Overwriting existing
passwords via this command is not permitted. This command does not change the
creation date of the original password but updates the _issued_ information.

**-h**, **\-\-help**

Print help for **copy**.

_**edit** \[\-\-generate RULES] PATH_
-------------------------------------

Edit an existing password in the store. Editing an non-existent password will
fail. passveil(1) starts *\$EDITOR* as input method before inserting the value.
Changing the value of a password will be visible to all _trusted_ gpg2(1) keys.

Since this commandchanges the password for everyone who once had access to it.
to it and is no longer trusted is cleared from the list of _insiders_.

Time and fingerprint if the gpg2(1) key that has been used to change the
password is recorded as _updated_.

**\-\-generate** RULES

Automatically generate a password according to a set of rules.

**-h**, **\-\-help**

Print help for **edit**.

QUERY OPERATIONS
----------------

Used to retrieve meta data and path information from the _index_. The _index_ is
created from decrypted information and is never shared publicly. None of these
operations require access to the private key.

_**show** PATH_
---------------

Show password of a key. If the store is signed passveil(1) is going to check if
the created file was signed using the gpg2(1) key that is claiming to have
issued the encrypted file.

**-h**, **\-\-help**

Print help for **show**.

_**list** \[\-\-tree] \[PATH]_
------------------------------

List all keys below a path. passveil(1) uses the _index_ to search for matching
keys by testing of the stated path is a prefix of each stored path.

**\-\-tree**

Use tree format. Keys are colored in *cyan*.

**-h**, **\-\-help**

Print help for **list**.

_**search** \[\-\-tree] REGEX_
------------------------------

List keys matching a regular expression. passveil(1) is using the _index_ to
find paths that match the regular expression.

**\-\-tree**

  Use tree format. Keys are colored in *cyan*.

**-h**, **\-\-help**

Print help for **show**.

_**info** PATH_
---------------

Show key information. Throughout the lifetime of a key, information is getting
tracked by passveil(1). This information can be accessed with this information.
Each time stamp consists of the time and date when on commandhas been issued as
well as the gpg2(1) key that was used to issue it.

* **created**

Creation of a password. This information does not change throughout the lifetime
of a key (even when moved to a different path).

* **issued**

Creation of the file that contains the password and meta data associated with
it. Changing meta data like trusted gpg2(1) keys results in the secret
information written to disk for each trusted key. In a signed store these files
will also be signed by the issuer of this command using their private key.

* **updated**

Last time a password got updated. This concerns the actual password and not meta
data. If no updated happened since the creation of the key, this information is
displayed.

* **trusted**

The list of gpg2(1) keys that are able to access the password. Whenever a
password is changed via **edit** every trusted key is able to access the updated
information.

* **insiders**

The list of gpg2(1) keys that once had access to the password. This also keeps
track of the keys that have since been denied access to the password, since they
might have seen it at some point in time. This information is used for the
**distrust** command, that helps identifying potentially compromised passwords.

* **log**

Every **allow** and **deny** command that has been issued on the password since
the last **edit** command happened. This log is used to compute the list of
_trusted_ gpg2(1) keys as well as the _insiders_ (keys that were able to decrypt
the password at some point in time, since the last **edit** command).

**-h**, **\-\-help**

Print help for **info**.

TRUST MANAGEMENT
----------------

Modify trust within the store. Anyone with access to a password can modify trust
by allowing or denying others to have access to it.

_**allow** PATH IDENTITY_
-------------------------

Allow password to be shared with others. Each identity stated in this command is
resolved to its gpg2(1) key and is allowed from this point in time to see the
password as well as all changes to it.

**-h**, **\-\-help**

Print help for **allow**.


_**deny** PATH IDENTITY_
------------------------

Deny password to be shared with others. Each identity stated in this command is
denied on to see the password of a path as well as all changes to it.

**-h**, **\-\-help**

Print help for **deny**.

_**distrust** IDENTITY_
-----------------------

List potentially compromised passwords. passveil(1) uses the _index_ to check if
any gpg2(1) keys associated with the stated identities are listed among the
_insiders_ of a password. Such passwords might have been seen by distrusted
identities at some point and time and it might be necessary to change them using
the **edit** command.

**-h**, **\-\-help**

Print help for **distrust**.

STORE MANAGEMENT
----------------

Creation and management of the store. Some of these operations invoke darcs(1)
to synchronize the repository with its remote copy.

_**init** \[\-\-untracked] \[\-\-unsigned] IDENTITY \[REMOTE]_
--------------------------------------------------------------

Initialize a new store. This requires the identity of the owner which is
resolved to a private gpg(2) key. By default repositories sign all created files
with the private key of the user issuing the command, to prevent this a
repository can be initialized using the \-\-unsigned switch. Additionally the
repository is tracked using darcs(1), this also can be prevented by using the
\-\-untracked switch. Both of these operations are useful for unshared stores
that are never shared with other users.

During initialization one can specify a remote repository to clone from.

**\-\-untracked**

Do not track store with version control.

**\-\-unsigned**

Do not sign keys.

**-h**, **\-\-help**

Print help for **init**.

_**sync** \[\-\-reindex] \[\-\-offline]_
----------------------------------------

Synchronize store. Uses darcs(1) _rebase pull_ followed by _push_ to synchronize
the local and remote repositories. Conflicting local patches are suspended by
darcs(1).


**\-\-reindex**

Force _index_ rebuild.

**\-\-offline**

Do not synchronize with remote repository.

**-h**, **\-\-help**

Print help for **init**.

_**undo**_
----------

Undo local changes. Uses darcs(1) _obliterate_ to discard local patches that are
not in the remote repository. All discarded information will be lost.

**-h**, **\-\-help**

Print help for **undo**.

FILES
=====

The default store directory resides at _$HOME/.passveil_, unless an alternative
store is specified using the \-\-store option. A store directory includes the
following files and directories:

**config.json**

Internal configuration format, containing the currently used private gpg2(1) key
and configuration information stated at initialization (e.g. if signing is
used).

**index.json**

The local _index_ containing decrypted metadata. This is used to quickly search
entries in the store. the Index is never shared publicly and only contains
information that can be decrypted using a trusted private gpg2(1) key.

**store**

Contains the shared information like encrypted passwords and signatures. Each
stored secret has its own directory named using the SHA256 of the path. This
directory contains encrypted Content information for each trusted gpg2(1) key as
well as an optional detached signature file if the Store is signed (which is the
default).

SEE ALSO
========

darcs(1), gpg2(1)

AUTHORS
=======

passveil(1) is developed by antei GmbH (https://antei.de).
