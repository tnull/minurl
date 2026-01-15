// Property-based tests for URL parsing based on WHATWG URL Standard
// https://url.spec.whatwg.org/

use minurl::Url;
use proptest::prelude::*;

/// Strategy for generating valid URL schemes.
/// Per WHATWG spec: scheme starts with ASCII alpha, followed by ASCII alphanumeric, '+', '-', or '.'
/// We generate lowercase schemes to ensure round-trip serialization works.
fn scheme_strategy() -> impl Strategy<Value = String> {
	// First char: lowercase letter
	let first_char = prop::sample::select(('a'..='z').collect::<Vec<_>>());
	// Rest: lowercase alphanumeric, '+', '-', '.'
	let rest_chars = prop::collection::vec(
		prop::sample::select(
			('a'..='z').chain('0'..='9').chain(['+', '-', '.'].into_iter()).collect::<Vec<_>>(),
		),
		0..8,
	);

	(first_char, rest_chars).prop_map(|(first, rest)| {
		let mut s = String::with_capacity(1 + rest.len());
		s.push(first);
		for c in rest {
			s.push(c);
		}
		s
	})
}

/// Strategy for generating valid host/domain names.
/// Simplified: alphanumeric labels separated by dots, no leading/trailing hyphens in labels.
fn host_strategy() -> impl Strategy<Value = String> {
	// A label is alphanumeric characters, optionally with hyphens in the middle
	let label = "[a-z0-9]([a-z0-9-]{0,10}[a-z0-9])?";
	// 1-3 labels separated by dots
	prop::string::string_regex(&format!("{}(\\.{})?(\\.{})?", label, label, label))
		.expect("valid regex")
		.prop_filter("non-empty host", |s| !s.is_empty())
}

/// Strategy for generating valid ports (u16).
fn port_strategy() -> impl Strategy<Value = Option<u16>> {
	prop::option::of(any::<u16>())
}

/// Strategy for generating valid URL path characters.
/// Path can contain unreserved chars, percent-encoded, sub-delims, ':', '@', '/'
/// Simplified: alphanumeric, '-', '.', '_', '~', '/'
fn path_strategy() -> impl Strategy<Value = String> {
	prop::option::of(prop::string::string_regex("/[a-zA-Z0-9_.~/-]{0,50}").expect("valid regex"))
		.prop_map(|opt| opt.unwrap_or_default())
}

/// Strategy for generating valid query strings.
/// Query can contain unreserved, percent-encoded, sub-delims, ':', '@', '/', '?'
/// Simplified: alphanumeric, '-', '.', '_', '~', '=', '&'
fn query_strategy() -> impl Strategy<Value = Option<String>> {
	prop::option::of(prop::string::string_regex("[a-zA-Z0-9_.~=&-]{1,30}").expect("valid regex"))
}

/// Strategy for generating valid fragment identifiers.
/// Similar character set to query.
fn fragment_strategy() -> impl Strategy<Value = Option<String>> {
	prop::option::of(prop::string::string_regex("[a-zA-Z0-9_.~-]{1,20}").expect("valid regex"))
}

/// Combined strategy that generates a complete valid URL and its expected components.
fn valid_url_strategy() -> impl Strategy<Value = ValidUrl> {
	(
		scheme_strategy(),
		host_strategy(),
		port_strategy(),
		path_strategy(),
		query_strategy(),
		fragment_strategy(),
	)
		.prop_map(|(scheme, host, port, path, query, fragment)| {
			let mut url_string = format!("{}://{}", scheme, host);

			if let Some(p) = port {
				url_string.push_str(&format!(":{}", p));
			}

			url_string.push_str(&path);

			if let Some(ref q) = query {
				url_string.push('?');
				url_string.push_str(q);
			}

			if let Some(ref f) = fragment {
				url_string.push('#');
				url_string.push_str(f);
			}

			ValidUrl { url_string, scheme, host, port, path, query, fragment }
		})
}

#[derive(Debug, Clone)]
struct ValidUrl {
	url_string: String,
	scheme: String,
	host: String,
	port: Option<u16>,
	path: String,
	query: Option<String>,
	fragment: Option<String>,
}

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
