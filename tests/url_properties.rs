// Property-based tests for URL parsing based on WHATWG URL Standard
// https://url.spec.whatwg.org/

mod common;

use common::valid_url_strategy;
use minurl::Url;
use proptest::prelude::*;

proptest! {
	/// Test that all generated valid URLs can be parsed successfully.
	#[test]
	fn valid_urls_parse_successfully(valid_url in valid_url_strategy()) {
		let result = Url::parse(&valid_url.url_string);
		prop_assert!(
			result.is_ok(),
			"Failed to parse valid URL: {} - Error: {:?}",
			valid_url.url_string,
			result.err()
		);
	}

	/// Test round-trip: as_str() returns the same string that was parsed.
	#[test]
	fn parse_as_str_roundtrip(valid_url in valid_url_strategy()) {
		let parsed = Url::parse(&valid_url.url_string).expect("should parse");
		prop_assert_eq!(
			parsed.as_str(),
			&valid_url.url_string,
			"Round-trip failed for URL"
		);
	}

	/// Test that scheme() returns the expected scheme.
	#[test]
	fn scheme_returns_expected_value(valid_url in valid_url_strategy()) {
		let parsed = Url::parse(&valid_url.url_string).expect("should parse");
		prop_assert_eq!(
			parsed.scheme(),
			&valid_url.scheme,
			"Scheme mismatch"
		);
	}

	/// Test that base_url() returns the expected host.
	#[test]
	fn base_url_returns_expected_value(valid_url in valid_url_strategy()) {
		let parsed = Url::parse(&valid_url.url_string).expect("should parse");
		prop_assert_eq!(
			parsed.base_url(),
			&valid_url.host,
			"Host mismatch"
		);
	}

	/// Test that port() returns the expected port.
	#[test]
	fn port_returns_expected_value(valid_url in valid_url_strategy()) {
		let parsed = Url::parse(&valid_url.url_string).expect("should parse");
		prop_assert_eq!(
			parsed.port(),
			valid_url.port,
			"Port mismatch"
		);
	}

	/// Test that path() returns the expected path.
	#[test]
	fn path_returns_expected_value(valid_url in valid_url_strategy()) {
		let parsed = Url::parse(&valid_url.url_string).expect("should parse");
		prop_assert_eq!(
			parsed.path(),
			&valid_url.path,
			"Path mismatch"
		);
	}

	/// Test that query() returns the expected query string.
	#[test]
	fn query_returns_expected_value(valid_url in valid_url_strategy()) {
		let parsed = Url::parse(&valid_url.url_string).expect("should parse");
		prop_assert_eq!(
			parsed.query(),
			valid_url.query.as_deref(),
			"Query mismatch"
		);
	}

	/// Test that fragment() returns the expected fragment.
	#[test]
	fn fragment_returns_expected_value(valid_url in valid_url_strategy()) {
		let parsed = Url::parse(&valid_url.url_string).expect("should parse");
		prop_assert_eq!(
			parsed.fragment(),
			valid_url.fragment.as_deref(),
			"Fragment mismatch"
		);
	}

	/// Test that path_segments() correctly splits the path.
	#[test]
	fn path_segments_splits_correctly(valid_url in valid_url_strategy()) {
		let parsed = Url::parse(&valid_url.url_string).expect("should parse");

		let path = &valid_url.path;
		let expected_segments: Vec<&str> = if path.is_empty() {
			vec![""]
		} else if path.starts_with('/') {
			path[1..].split('/').collect()
		} else {
			path.split('/').collect()
		};

		let actual_segments: Vec<&str> = parsed.path_segments().collect();
		prop_assert_eq!(
			actual_segments,
			expected_segments,
			"Path segments mismatch"
		);
	}

	/// Test Display implementation matches as_str().
	#[test]
	fn display_matches_as_str(valid_url in valid_url_strategy()) {
		let parsed = Url::parse(&valid_url.url_string).expect("should parse");
		prop_assert_eq!(
			format!("{}", parsed),
			parsed.as_str(),
			"Display doesn't match as_str()"
		);
	}

	/// Test that parsing is deterministic - same input always produces same output.
	#[test]
	fn parsing_is_deterministic(valid_url in valid_url_strategy()) {
		let parsed1 = Url::parse(&valid_url.url_string).expect("should parse");
		let parsed2 = Url::parse(&valid_url.url_string).expect("should parse");
		prop_assert_eq!(parsed1, parsed2, "Parsing is not deterministic");
	}

	/// Test query_pairs() correctly parses query parameters.
	#[test]
	fn query_pairs_parses_correctly(
		valid_url in valid_url_strategy()
			.prop_filter("has query", |u| u.query.is_some())
	) {
		let parsed = Url::parse(&valid_url.url_string).expect("should parse");
		let query = valid_url.query.as_ref().unwrap();

		let expected_pairs: Vec<(&str, &str)> = query
			.split('&')
			.map(|pair| {
				if let Some(eq_pos) = pair.find('=') {
					(&pair[..eq_pos], &pair[eq_pos + 1..])
				} else {
					(pair, "")
				}
			})
			.collect();

		let actual_pairs: Vec<(&str, &str)> = parsed.query_pairs().collect();
		prop_assert_eq!(
			actual_pairs,
			expected_pairs,
			"Query pairs mismatch"
		);
	}
}

// Additional edge case tests using proptest for schemes with special characters.
proptest! {
	#[test]
	fn scheme_with_special_chars_parses(
		base in "[a-z]",
		special in prop::sample::select(vec!['+', '-', '.']),
		suffix in "[a-z0-9]{0,3}"
	) {
		let scheme = format!("{}{}{}", base, special, suffix);
		let url_string = format!("{}://example.com", scheme);
		let parsed = Url::parse(&url_string);
		prop_assert!(
			parsed.is_ok(),
			"Failed to parse URL with scheme '{}': {:?}",
			scheme,
			parsed.as_ref().err()
		);
		let parsed = parsed.unwrap();
		prop_assert_eq!(parsed.scheme(), scheme);
	}
}
