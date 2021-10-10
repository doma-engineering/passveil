#Passveil

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
