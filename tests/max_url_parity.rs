// Parity tests comparing minurl::Url with url::Url (MaxUrl)
// Ensures our implementation matches the behavior of the reference url crate
// for all public API methods that exist on both types.
//
// Note: We use "special" schemes (http, https, ws, wss, ftp) for parity testing
// because the url crate treats non-special schemes differently (as opaque paths).

mod common;

use common::special_url_string_strategy;
use minurl::Url as MinUrl;
use proptest::prelude::*;
use url::Url as MaxUrl;

proptest! {
	/// Test that scheme() returns the same value for both implementations.
	#[test]
	fn scheme_parity(url_string in special_url_string_strategy()) {
		let min_url = MinUrl::parse(&url_string).expect("minurl should parse");
		let max_url = MaxUrl::parse(&url_string).expect("url crate should parse");

		prop_assert_eq!(
			min_url.scheme(),
			max_url.scheme(),
			"scheme() mismatch for URL: {}",
			url_string
		);
	}

	/// Test that port() returns the same value for both implementations.
	#[test]
	fn port_parity(url_string in special_url_string_strategy()) {
		let min_url = MinUrl::parse(&url_string).expect("minurl should parse");
		let max_url = MaxUrl::parse(&url_string).expect("url crate should parse");

		prop_assert_eq!(
			min_url.port(),
			max_url.port(),
			"port() mismatch for URL: {}",
			url_string
		);
	}

	/// Test that port_or_known_default() returns the same value for both implementations.
	#[test]
	fn port_or_known_default_parity(url_string in special_url_string_strategy()) {
		let min_url = MinUrl::parse(&url_string).expect("minurl should parse");
		let max_url = MaxUrl::parse(&url_string).expect("url crate should parse");

		prop_assert_eq!(
			min_url.port_or_known_default(),
			max_url.port_or_known_default(),
			"port_or_known_default() mismatch for URL: {}",
			url_string
		);
	}

	/// Test that username() returns the same value for both implementations.
	#[test]
	fn username_parity(url_string in special_url_string_strategy()) {
		let min_url = MinUrl::parse(&url_string).expect("minurl should parse");
		let max_url = MaxUrl::parse(&url_string).expect("url crate should parse");

		prop_assert_eq!(
			min_url.username(),
			max_url.username(),
			"username() mismatch for URL: {}",
			url_string
		);
	}

	/// Test that password() returns the same value for both implementations.
	#[test]
	fn password_parity(url_string in special_url_string_strategy()) {
		let min_url = MinUrl::parse(&url_string).expect("minurl should parse");
		let max_url = MaxUrl::parse(&url_string).expect("url crate should parse");

		prop_assert_eq!(
			min_url.password(),
			max_url.password(),
			"password() mismatch for URL: {}",
			url_string
		);
	}

	/// Test that path() returns the same value for both implementations.
	/// Note: url crate normalizes empty paths to "/" for URLs with authority,
	/// while minurl returns "". We test that they match for non-empty paths.
	#[test]
	fn path_parity(url_string in special_url_string_strategy()) {
		let min_url = MinUrl::parse(&url_string).expect("minurl should parse");
		let max_url = MaxUrl::parse(&url_string).expect("url crate should parse");

		let min_path = min_url.path();
		let max_path = max_url.path();

		// url crate adds "/" for empty paths, minurl returns ""
		// Both are valid interpretations, so we normalize for comparison
		let min_normalized = if min_path.is_empty() { "/" } else { min_path };

		prop_assert_eq!(
			min_normalized,
			max_path,
			"path() mismatch for URL: {} (minurl: '{}', url: '{}')",
			url_string,
			min_path,
			max_path
		);
	}

	/// Test that query() returns the same value for both implementations.
	#[test]
	fn query_parity(url_string in special_url_string_strategy()) {
		let min_url = MinUrl::parse(&url_string).expect("minurl should parse");
		let max_url = MaxUrl::parse(&url_string).expect("url crate should parse");

		prop_assert_eq!(
			min_url.query(),
			max_url.query(),
			"query() mismatch for URL: {}",
			url_string
		);
	}

	/// Test that fragment() returns the same value for both implementations.
	#[test]
	fn fragment_parity(url_string in special_url_string_strategy()) {
		let min_url = MinUrl::parse(&url_string).expect("minurl should parse");
		let max_url = MaxUrl::parse(&url_string).expect("url crate should parse");

		prop_assert_eq!(
			min_url.fragment(),
			max_url.fragment(),
			"fragment() mismatch for URL: {}",
			url_string
		);
	}

	/// Test that host (base_url vs host_str) returns the same value.
	/// minurl::Url::base_url() corresponds to url::Url::host_str().
	#[test]
	fn host_parity(url_string in special_url_string_strategy()) {
		let min_url = MinUrl::parse(&url_string).expect("minurl should parse");
		let max_url = MaxUrl::parse(&url_string).expect("url crate should parse");

		prop_assert_eq!(
			Some(min_url.base_url()),
			max_url.host_str(),
			"host mismatch for URL: {} (minurl base_url: '{}', url host_str: {:?})",
			url_string,
			min_url.base_url(),
			max_url.host_str()
		);
	}

	/// Test that as_str() returns equivalent URLs.
	/// Note: The url crate normalizes empty paths to "/" so we account for that.
	#[test]
	fn as_str_parity(url_string in special_url_string_strategy()) {
		let min_url = MinUrl::parse(&url_string).expect("minurl should parse");
		let max_url = MaxUrl::parse(&url_string).expect("url crate should parse");

		// Both should produce parseable URLs that round-trip correctly
		let min_str = min_url.as_str();
		let max_str = max_url.as_str();

		// Re-parse to ensure both produce valid URLs
		let min_reparsed = MinUrl::parse(min_str);
		let max_reparsed = MaxUrl::parse(max_str);

		prop_assert!(
			min_reparsed.is_ok(),
			"minurl as_str() produced unparseable URL: {}",
			min_str
		);
		prop_assert!(
			max_reparsed.is_ok(),
			"url crate as_str() produced unparseable URL: {}",
			max_str
		);

		// The serialized forms should be semantically equivalent
		// (they may differ in empty path normalization)
		let min_reparsed = min_reparsed.unwrap();
		let max_reparsed = max_reparsed.unwrap();

		prop_assert_eq!(
			min_reparsed.scheme(),
			max_reparsed.scheme(),
			"Reparsed scheme mismatch"
		);
		prop_assert_eq!(
			min_reparsed.port(),
			max_reparsed.port(),
			"Reparsed port mismatch"
		);
		prop_assert_eq!(
			min_reparsed.query(),
			max_reparsed.query(),
			"Reparsed query mismatch"
		);
		prop_assert_eq!(
			min_reparsed.fragment(),
			max_reparsed.fragment(),
			"Reparsed fragment mismatch"
		);
	}

	/// Test that path_segments() returns the same segments for both implementations.
	/// Note: url crate's path_segments() returns None for cannot-be-a-base URLs,
	/// but our generated URLs always have authority so this shouldn't happen.
	#[test]
	fn path_segments_parity(url_string in special_url_string_strategy()) {
		let min_url = MinUrl::parse(&url_string).expect("minurl should parse");
		let max_url = MaxUrl::parse(&url_string).expect("url crate should parse");

		let min_segments: Vec<&str> = min_url.path_segments().collect();
		let max_segments: Option<Vec<&str>> = max_url.path_segments().map(|s| s.collect());

		// url crate should always return Some for URLs with authority
		prop_assert!(
			max_segments.is_some(),
			"url crate returned None for path_segments on URL with authority: {}",
			url_string
		);

		let max_segments = max_segments.unwrap();

		// Handle the empty path case: minurl returns [""], url crate returns [""]
		// for "/" path (which url crate normalizes empty paths to)
		prop_assert_eq!(
			min_segments,
			max_segments,
			"path_segments() mismatch for URL: {}",
			url_string
		);
	}

	/// Test that Display output matches as_str() for both implementations.
	#[test]
	fn display_parity(url_string in special_url_string_strategy()) {
		let min_url = MinUrl::parse(&url_string).expect("minurl should parse");
		let max_url = MaxUrl::parse(&url_string).expect("url crate should parse");

		// Both should have Display == as_str()
		prop_assert_eq!(
			format!("{}", min_url),
			min_url.as_str(),
			"minurl Display doesn't match as_str()"
		);
		prop_assert_eq!(
			format!("{}", max_url),
			max_url.as_str(),
			"url crate Display doesn't match as_str()"
		);
	}
}

// Test that both implementations accept or reject the same URLs
proptest! {
	/// Test that valid URLs are accepted by both implementations.
	#[test]
	fn both_accept_valid_urls(url_string in special_url_string_strategy()) {
		let min_result = MinUrl::parse(&url_string);
		let max_result = MaxUrl::parse(&url_string);

		prop_assert!(
			min_result.is_ok(),
			"minurl rejected valid URL: {} - {:?}",
			url_string,
			min_result.err()
		);
		prop_assert!(
			max_result.is_ok(),
			"url crate rejected valid URL: {} - {:?}",
			url_string,
			max_result.err()
		);
	}
}
