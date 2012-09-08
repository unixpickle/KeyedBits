# Usage

## The KBObject data type

HKeyedBits makes it as easy as possible to encode and decode general KeyedBits objects. However, Haskell's built-in types are too strict to represent the loose data structures which can be formed in the KeyedBits specification. For this reason, HKeyedBits includes an easy-to-use data type (which can be found in `KeyedBits.Object`) called `KBObject`.

The `KBObject` data type includes a constructor for an integer (`KBInteger`), a string (`KBString`), and the various other kinds of KeyedBits objects. The use of the `KBObject` data type makes it possible to represent every possible structure of KeyedBits data while remaining to be type-safe.

It is easy to construct `KBObject`s. `KeyedBits.Object` includes the functions `packHash` and `packArray` which act as convenience constructors. For example, a dictionary `{"count": 10, "name": "alex"}` could be constructed as follows:

    packHash [("count", KBInt 10),("name", KBString "alex")]

In addition, `KeyedBits.Object` includes various functions for converting `KBObject`s back to built-in Haskell types. These functions include `unpackArray`, `unpackInt`, and so forth. These functions all return `Maybe` data to provide error feedback when the wrong type has been specified (e.g. `unpackArray` was called on a `KBString`).

## Encoding to a Lazy ByteString

HKeyedBits facilitates all of its encoding functions within `KeyedBits.Encode`. The module itself exports one function: `encode`. The function is typed as follows:

    encode :: KeyedBits.Object.KBObject -> Data.ByteString.Lazy.ByteString

Since `KBObject` represents a perfectly crafted KeyedBits object, it is impossible for an encode to fail; thus, the `encode` function has no need to wrap the return value in any sort of failure Monad.

## Decoding from a Lazy ByteString

The decoding facility resides in, as you may have guessed, `KeyedBits.Decode`. However, whereas `KeyedBits.Encode` exports only a single function, `Decode` has a little bit more to it than that.

When decoding from a ByteString, the `Decode` module simply reads the *next* KeyedBits object from the buffer, returning the remaining data along with the decoded `KBObject` result. While this makes it possible to decode multiple KeyedBits objects from one data source, it also adds an additional cost: a decode is somewhat stateful.

Because of this, `KeyedBits.Decode` uses an internal type called DecodeState to keep track of the remaining BinaryString while also returning values between functions. All this really means is that the `runDecodeState` function must be used in conjunction with the `readObject` function as follows:

    let (obj, remaining) <- runDecodeState readObject buffer

In this case, `buffer` is the initial piece of data to be decoded, `obj` is the resulting `KBObject`, and `remaining` is the residual binary data that was left over after the decode. In the case of a decode error, an exception will be thrown.

## Decoding from a Handle

HKeyedBits also makes it possible to decode a KeyedBits object directly from a `Handle`. This makes it possible not only to read directly from a file, but also to read a sort of "stream" of KeyedBits objects from a pipe or network socket.

The `KeyedBits.Read` module facilitates `Handle` decoding, exporting a single `readObject` function. This function's type is as follows:

    readObject :: Handle -> IO KBObject

This function behaves in the same way as the `readObject` function from `KeyedBits.Decode`, except that there is no residual data to be returned. In the case of a decode error, an exception will be thrown.

# Why Would I Write This?

Insanity is probably a major factor. As it turns out (and as those of you who are more experienced in Haskell may have guessed), this is my first ever Haskell project. At the time of writing this library, I had known haskell for little more than a week. I figured that a KeyedBits implementation was a fitting learning experience, since it would ease me into various components of the language such as the type system and the idea of Monads.
