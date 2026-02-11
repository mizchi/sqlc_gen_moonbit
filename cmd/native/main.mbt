// Native entry point for sqlc-gen-moonbit

///|
fn main {
  // Read stdin
  let input = read_all_stdin()

  // Process request using codegen library
  let output = @codegen.process_request(input)

  // Write to stdout
  write_all_stdout(output)
}
