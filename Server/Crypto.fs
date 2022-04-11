module Crypto

open System.Security.Cryptography

/// Create a random salt
let salt () =
    let rng = RandomNumberGenerator.Create()
    let salt = Array.create 32 (byte 0)
    rng.GetBytes(salt)
    salt

/// Hashes the given plaintext password with the given salt
let sha256 (input: string) (salt: byte []) =
    let iterations = 20000

    let pbkdf2 =
        new Rfc2898DeriveBytes(input, salt, iterations, HashAlgorithmName.SHA256)

    pbkdf2.GetBytes(32)
