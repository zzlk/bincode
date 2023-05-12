#![cfg(all(feature = "serde", feature = "alloc", feature = "derive"))]

extern crate alloc;

use alloc::string::String;
use bytes::{Bytes, BytesMut};
use serde::de::Visitor;
use serde_derive::{Deserialize, Serialize};
use std::error::Error;

#[derive(Serialize, Deserialize, bincode::Encode, bincode::Decode)]
pub struct SerdeRoundtrip {
    pub a: u32,
    #[serde(skip)]
    pub b: u32,
    pub c: TupleS,
}

#[derive(Serialize, Deserialize, bincode::Encode, bincode::Decode, PartialEq, Debug)]
pub struct TupleS(f32, f32, f32);

#[test]
fn test_serde_round_trip() {
    // validate serde attribute working
    let json = serde_json::to_string(&SerdeRoundtrip {
        a: 5,
        b: 5,
        c: TupleS(2.0, 3.0, 4.0),
    })
    .unwrap();
    assert_eq!("{\"a\":5,\"c\":[2.0,3.0,4.0]}", json);

    let result: SerdeRoundtrip = serde_json::from_str(&json).unwrap();
    assert_eq!(result.a, 5);
    assert_eq!(result.b, 0);

    // validate bincode working
    let bytes = bincode::serde::encode_to_vec(
        SerdeRoundtrip {
            a: 15,
            b: 15,
            c: TupleS(2.0, 3.0, 4.0),
        },
        bincode::config::standard(),
    )
    .unwrap();
    assert_eq!(bytes, &[15, 0, 0, 0, 64, 0, 0, 64, 64, 0, 0, 128, 64]);
    let (result, len): (SerdeRoundtrip, usize) =
        bincode::serde::decode_from_slice(&bytes, bincode::config::standard()).unwrap();
    assert_eq!(result.a, 15);
    assert_eq!(result.b, 0); // remember: b is skipped
    assert_eq!(result.c, TupleS(2.0, 3.0, 4.0));
    assert_eq!(len, 13);
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
pub struct SerdeWithBorrowedData<'a> {
    pub a: u32,
    #[serde(skip)]
    pub b: u32,
    pub str: &'a str,
}

#[test]
fn test_serialize_deserialize_borrowed_data() {
    let input = SerdeWithBorrowedData {
        a: 5,
        b: 5,
        str: "Hello world",
    };

    #[rustfmt::skip]
    let expected = &[
        5, // a
        // b is skipped
        11, // str length
        b'H', b'e', b'l', b'l', b'o', b' ', b'w', b'o', b'r', b'l', b'd' // str
    ];

    let mut result = [0u8; 20];
    let len = bincode::serde::encode_into_slice(&input, &mut result, bincode::config::standard())
        .unwrap();
    let result = &result[..len];
    assert_eq!(result, expected);

    let result = bincode::serde::encode_to_vec(&input, bincode::config::standard()).unwrap();

    assert_eq!(result, expected);

    let output: SerdeWithBorrowedData =
        bincode::serde::decode_borrowed_from_slice(&result, bincode::config::standard()).unwrap();
    assert_eq!(
        SerdeWithBorrowedData {
            b: 0, // remember: b is skipped
            ..input
        },
        output
    );
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
pub struct SerdeWithOwnedData {
    pub a: u32,
    #[serde(skip)]
    pub b: u32,
    pub str: String,
}

#[test]
fn test_serialize_deserialize_owned_data() {
    let input = SerdeWithOwnedData {
        a: 5,
        b: 5,
        str: String::from("Hello world"),
    };

    #[rustfmt::skip]
    let expected = &[
        5, // a
        // b is skipped
        11, // str length
        b'H', b'e', b'l', b'l', b'o', b' ', b'w', b'o', b'r', b'l', b'd' // str
    ];

    let mut result = [0u8; 20];
    let len = bincode::serde::encode_into_slice(&input, &mut result, bincode::config::standard())
        .unwrap();
    let result = &result[..len];
    assert_eq!(result, expected);

    let result = bincode::serde::encode_to_vec(&input, bincode::config::standard()).unwrap();

    assert_eq!(result, expected);

    let (output, len): (SerdeWithOwnedData, usize) =
        bincode::serde::decode_from_slice(&result, bincode::config::standard()).unwrap();
    assert_eq!(
        SerdeWithOwnedData {
            b: 0, // remember: b is skipped
            ..input
        },
        output
    );
    assert_eq!(len, 13);
}

#[test]
fn test_bytes_round_trip() {
    use serde::Deserialize;
    use serde::Serialize;

    #[derive(Clone, Debug, PartialEq, Eq)]
    struct BytesWrapper(pub Bytes);
    impl Serialize for BytesWrapper {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            serializer.serialize_bytes(&self.0)
        }
    }

    impl<'de> Deserialize<'de> for BytesWrapper {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            struct V;
            impl Visitor<'_> for V {
                type Value = Bytes;

                fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                    formatter.write_str(std::any::type_name::<Self::Value>())
                }

                fn visit_bytes_bytes<E>(self, v: Bytes) -> Result<Self::Value, E>
                where
                    E: Error,
                {
                    Ok(v)
                }
            }

            Ok(BytesWrapper(deserializer.deserialize_bytes(V)?))
        }
    }

    #[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
    pub struct SerdeWithOwnedData {
        pub bytes: BytesWrapper,
    }

    let input = SerdeWithOwnedData {
        bytes: BytesWrapper(Bytes::from_static(&[7])),
    };

    #[rustfmt::skip]
    let expected = [
        1,
        7
    ].as_slice();

    let mut result = BytesMut::from_iter(&[0u8; 20]);
    let len = bincode::serde::encode_into_slice(&input, &mut result, bincode::config::standard())
        .unwrap();
    let result = result.split_to(len).freeze();
    assert_eq!(result, expected);

    let (output, len): (SerdeWithOwnedData, usize) =
        bincode::serde::decode_from_bytes(result.clone(), bincode::config::standard()).unwrap();
    assert_eq!(input, output);
    assert_eq!(len, 2);
}

#[cfg(feature = "derive")]
mod derive {
    use bincode::{Decode, Encode};
    use serde_derive::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
    pub struct SerdeType {
        pub a: u32,
    }

    #[derive(Decode, Encode, PartialEq, Eq, Debug)]
    pub struct StructWithSerde {
        #[bincode(with_serde)]
        pub serde: SerdeType,
    }

    #[derive(Decode, Encode, PartialEq, Eq, Debug)]
    pub enum EnumWithSerde {
        Unit(#[bincode(with_serde)] SerdeType),
        Struct {
            #[bincode(with_serde)]
            serde: SerdeType,
        },
    }

    #[test]
    fn test_serde_derive() {
        fn test_encode_decode<T>(start: T, expected_len: usize)
        where
            T: bincode::Encode + bincode::Decode + PartialEq + core::fmt::Debug,
        {
            let mut slice = [0u8; 100];
            let len = bincode::encode_into_slice(&start, &mut slice, bincode::config::standard())
                .unwrap();
            assert_eq!(len, expected_len);
            let slice = &slice[..len];
            let (result, len): (T, usize) =
                bincode::decode_from_slice(slice, bincode::config::standard()).unwrap();

            assert_eq!(start, result);
            assert_eq!(len, expected_len);
        }
        test_encode_decode(
            StructWithSerde {
                serde: SerdeType { a: 5 },
            },
            1,
        );
        test_encode_decode(EnumWithSerde::Unit(SerdeType { a: 5 }), 2);
        test_encode_decode(
            EnumWithSerde::Struct {
                serde: SerdeType { a: 5 },
            },
            2,
        );
    }
}
