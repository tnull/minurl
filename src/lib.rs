// This file is Copyright its original authors, visible in version control history.
//
// This file is licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. You may not use this file except in
// accordance with one or both of these licenses.

#![crate_name = "minurl"]
#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]
#![allow(bare_trait_objects)]
#![allow(ellipsis_inclusive_range_patterns)]
#![cfg_attr(docsrs, feature(doc_cfg))]

//! A minimal library for parsing and validating URLs.

/// Errors that can occur during URL parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
	/// The input string is empty.
	EmptyInput,
	/// The input contains invalid characters (control characters or non-ASCII).
	InvalidCharacter(char),
	/// The URL is missing a scheme.
	MissingScheme,
	/// The URL has an invalid scheme format.
	InvalidScheme,
	/// The port number is invalid.
	InvalidPort,
}

/// A parsed URL.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Url {
	serialization: String,
	scheme: String,
	base_url: String,
	port: Option<u16>,
	path: String,
	query: Option<String>,
	fragment: Option<String>,
}

impl Url {
	/// Parses a URL string and returns a `Url` instance.
	///
	/// Validates that the input contains only valid non-control ASCII characters.
	pub fn parse(url_str: &str) -> Result<Self, ParseError> {
		if url_str.is_empty() {
			return Err(ParseError::EmptyInput);
		}

		// Validate: only non-control ASCII characters allowed
		for c in url_str.chars() {
			if !c.is_ascii() || c.is_ascii_control() {
				return Err(ParseError::InvalidCharacter(c));
			}
		}

		// Find the scheme (everything before "://")
		let scheme_end = url_str.find("://").ok_or(ParseError::MissingScheme)?;

		if scheme_end == 0 {
			return Err(ParseError::InvalidScheme);
		}

		let scheme = &url_str[..scheme_end];

		// Validate scheme: must start with a letter and contain only
		// letters, digits, '+', '-', or '.'
		let mut scheme_chars = scheme.chars();
		let first_char = scheme_chars.next().ok_or(ParseError::InvalidScheme)?;
		if !first_char.is_ascii_alphabetic() {
			return Err(ParseError::InvalidScheme);
		}

		for c in scheme_chars {
			if !c.is_ascii_alphanumeric() && c != '+' && c != '-' && c != '.' {
				return Err(ParseError::InvalidScheme);
			}
		}

		// Parse the rest after "://"
		let after_scheme = &url_str[scheme_end + 3..];

		// Extract the authority (host:port) - everything before '/', '?', or '#'
		let authority_end =
			after_scheme.find(|c| c == '/' || c == '?' || c == '#').unwrap_or(after_scheme.len());

		let authority = &after_scheme[..authority_end];
		let after_authority = &after_scheme[authority_end..];

		// Extract the path - everything from '/' until '?' or '#'
		let (path, after_path) = if after_authority.starts_with('/') {
			let path_end =
				after_authority.find(|c| c == '?' || c == '#').unwrap_or(after_authority.len());
			(&after_authority[..path_end], &after_authority[path_end..])
		} else {
			("", after_authority)
		};

		// Extract the query - everything after '?' until '#'
		let (query, after_query) = if after_path.starts_with('?') {
			let query_end = after_path[1..].find('#').map(|i| i + 1).unwrap_or(after_path.len());
			(Some(&after_path[1..query_end]), &after_path[query_end..])
		} else {
			(None, after_path)
		};

		// Extract the fragment - everything after '#'
		let fragment = if after_query.starts_with('#') { Some(&after_query[1..]) } else { None };

		// Parse host and optional port from authority
		let (host, port) = if let Some(colon_pos) = authority.rfind(':') {
			let potential_port = &authority[colon_pos + 1..];
			// Check if this is actually a port (all digits) or part of IPv6
			if !potential_port.is_empty()
				&& potential_port.chars().all(|c| c.is_ascii_digit())
				&& !authority.starts_with('[')
			{
				let port_num: u16 = potential_port.parse().map_err(|_| ParseError::InvalidPort)?;
				(&authority[..colon_pos], Some(port_num))
			} else {
				(authority, None)
			}
		} else {
			(authority, None)
		};

		let scheme = scheme.to_lowercase();
		let path = path.to_string();
		let query = query.map(|q| q.to_string());
		let fragment = fragment.map(|f| f.to_string());

		// Build the serialized URL
		let mut serialization = format!("{}://{}", scheme, host);
		if let Some(p) = port {
			serialization.push_str(&format!(":{}", p));
		}
		serialization.push_str(&path);
		if let Some(ref q) = query {
			serialization.push('?');
			serialization.push_str(q);
		}
		if let Some(ref f) = fragment {
			serialization.push('#');
			serialization.push_str(f);
		}

		Ok(Url { serialization, scheme, base_url: host.to_string(), port, path, query, fragment })
	}

	/// Returns the scheme of the URL (e.g., "http", "https").
	pub fn scheme(&self) -> &str {
		&self.scheme
	}

	/// Returns the base URL (host portion).
	pub fn base_url(&self) -> &str {
		&self.base_url
	}

	/// Returns the port number if specified.
	pub fn port(&self) -> Option<u16> {
		self.port
	}

	/// Returns the path of the URL.
	///
	/// The path includes the leading `/` if present. Returns an empty string
	/// if no path was specified.
	pub fn path(&self) -> &str {
		&self.path
	}

	/// Returns an iterator over the path segments.
	///
	/// Path segments are the portions between `/` characters. Empty segments
	/// (from leading or consecutive slashes) are included.
	pub fn path_segments(&self) -> impl Iterator<Item = &str> {
		let path = if self.path.starts_with('/') { &self.path[1..] } else { &self.path[..] };
		path.split('/')
	}

	/// Returns the query string of the URL, if present.
	///
	/// The returned string does not include the leading `?`.
	pub fn query(&self) -> Option<&str> {
		self.query.as_deref()
	}

	/// Returns an iterator over the query string's key-value pairs.
	///
	/// Pairs are separated by `&` and keys are separated from values by `=`.
	/// If a pair has no `=`, the value will be an empty string.
	pub fn query_pairs(&self) -> impl Iterator<Item = (&str, &str)> {
		self.query.as_deref().into_iter().flat_map(|q| {
			q.split('&').map(|pair| {
				if let Some(eq_pos) = pair.find('=') {
					(&pair[..eq_pos], &pair[eq_pos + 1..])
				} else {
					(pair, "")
				}
			})
		})
	}

	/// Returns the fragment identifier of the URL, if present.
	///
	/// The returned string does not include the leading `#`.
	pub fn fragment(&self) -> Option<&str> {
		self.fragment.as_deref()
	}

	/// Returns the serialized URL as a string slice.
	pub fn as_str(&self) -> &str {
		&self.serialization
	}
}

impl std::fmt::Display for Url {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(self.as_str())
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn parse_simple_url() {
		let url = Url::parse("http://example.com").unwrap();
		assert_eq!(url.scheme(), "http");
		assert_eq!(url.base_url(), "example.com");
		assert_eq!(url.port(), None);
	}

	#[test]
	fn parse_url_with_port() {
		let url = Url::parse("https://example.com:8080").unwrap();
		assert_eq!(url.scheme(), "https");
		assert_eq!(url.base_url(), "example.com");
		assert_eq!(url.port(), Some(8080));
	}

	#[test]
	fn parse_url_with_path() {
		let url = Url::parse("http://example.com/path/to/resource").unwrap();
		assert_eq!(url.scheme(), "http");
		assert_eq!(url.base_url(), "example.com");
		assert_eq!(url.port(), None);
	}

	#[test]
	fn empty_input_returns_error() {
		assert_eq!(Url::parse(""), Err(ParseError::EmptyInput));
	}

	#[test]
	fn missing_scheme_returns_error() {
		assert_eq!(Url::parse("example.com"), Err(ParseError::MissingScheme));
	}

	#[test]
	fn invalid_character_returns_error() {
		// Control character
		assert!(matches!(
			Url::parse("http://example\x00.com"),
			Err(ParseError::InvalidCharacter('\x00'))
		));

		// Non-ASCII character
		assert!(matches!(Url::parse("http://exämple.com"), Err(ParseError::InvalidCharacter('ä'))));
	}

	#[test]
	fn scheme_is_lowercased() {
		let url = Url::parse("HTTP://EXAMPLE.COM").unwrap();
		assert_eq!(url.scheme(), "http");
	}

	#[test]
	fn path_returns_full_path() {
		let url = Url::parse("http://example.com/path/to/resource").unwrap();
		assert_eq!(url.path(), "/path/to/resource");
	}

	#[test]
	fn path_is_empty_when_not_specified() {
		let url = Url::parse("http://example.com").unwrap();
		assert_eq!(url.path(), "");
	}

	#[test]
	fn path_segments_splits_correctly() {
		let url = Url::parse("http://example.com/path/to/resource").unwrap();
		let segments: Vec<&str> = url.path_segments().collect();
		assert_eq!(segments, vec!["path", "to", "resource"]);
	}

	#[test]
	fn path_segments_handles_empty_path() {
		let url = Url::parse("http://example.com").unwrap();
		let segments: Vec<&str> = url.path_segments().collect();
		assert_eq!(segments, vec![""]);
	}

	#[test]
	fn path_stops_at_query_string() {
		let url = Url::parse("http://example.com/path?query=value").unwrap();
		assert_eq!(url.path(), "/path");
	}

	#[test]
	fn path_stops_at_fragment() {
		let url = Url::parse("http://example.com/path#section").unwrap();
		assert_eq!(url.path(), "/path");
	}

	#[test]
	fn query_returns_query_string() {
		let url = Url::parse("http://example.com/path?foo=bar&baz=qux").unwrap();
		assert_eq!(url.query(), Some("foo=bar&baz=qux"));
	}

	#[test]
	fn query_is_none_when_not_present() {
		let url = Url::parse("http://example.com/path").unwrap();
		assert_eq!(url.query(), None);
	}

	#[test]
	fn query_stops_at_fragment() {
		let url = Url::parse("http://example.com/path?query=value#section").unwrap();
		assert_eq!(url.query(), Some("query=value"));
	}

	#[test]
	fn query_pairs_parses_key_value_pairs() {
		let url = Url::parse("http://example.com?foo=bar&baz=qux").unwrap();
		let pairs: Vec<(&str, &str)> = url.query_pairs().collect();
		assert_eq!(pairs, vec![("foo", "bar"), ("baz", "qux")]);
	}

	#[test]
	fn query_pairs_handles_missing_value() {
		let url = Url::parse("http://example.com?foo&bar=baz").unwrap();
		let pairs: Vec<(&str, &str)> = url.query_pairs().collect();
		assert_eq!(pairs, vec![("foo", ""), ("bar", "baz")]);
	}

	#[test]
	fn query_pairs_is_empty_when_no_query() {
		let url = Url::parse("http://example.com").unwrap();
		let pairs: Vec<(&str, &str)> = url.query_pairs().collect();
		assert!(pairs.is_empty());
	}

	#[test]
	fn fragment_returns_fragment() {
		let url = Url::parse("http://example.com/path#section").unwrap();
		assert_eq!(url.fragment(), Some("section"));
	}

	#[test]
	fn fragment_is_none_when_not_present() {
		let url = Url::parse("http://example.com/path").unwrap();
		assert_eq!(url.fragment(), None);
	}

	#[test]
	fn fragment_with_query() {
		let url = Url::parse("http://example.com/path?query=value#section").unwrap();
		assert_eq!(url.query(), Some("query=value"));
		assert_eq!(url.fragment(), Some("section"));
	}

	#[test]
	fn fragment_without_path_or_query() {
		let url = Url::parse("http://example.com#section").unwrap();
		assert_eq!(url.path(), "");
		assert_eq!(url.query(), None);
		assert_eq!(url.fragment(), Some("section"));
	}

	#[test]
	fn as_str_returns_full_url() {
		let url = Url::parse("http://example.com/path?query=value#section").unwrap();
		assert_eq!(url.as_str(), "http://example.com/path?query=value#section");
	}

	#[test]
	fn as_str_with_port() {
		let url = Url::parse("https://example.com:8080/path").unwrap();
		assert_eq!(url.as_str(), "https://example.com:8080/path");
	}

	#[test]
	fn as_str_normalizes_scheme_to_lowercase() {
		let url = Url::parse("HTTP://EXAMPLE.COM/path").unwrap();
		assert_eq!(url.as_str(), "http://EXAMPLE.COM/path");
	}

	#[test]
	fn as_str_minimal_url() {
		let url = Url::parse("http://example.com").unwrap();
		assert_eq!(url.as_str(), "http://example.com");
	}

	#[test]
	fn display_matches_as_str() {
		let url = Url::parse("http://example.com/path?query=value#section").unwrap();
		assert_eq!(format!("{}", url), url.as_str());
	}

	#[test]
	fn display_can_be_used_in_format_string() {
		let url = Url::parse("http://example.com").unwrap();
		let formatted = format!("URL: {}", url);
		assert_eq!(formatted, "URL: http://example.com");
	}
}
