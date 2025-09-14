# cl-coverage-tools

![cl-coverage-tools](https://github.com/cl-sdk/cl-coverage-tools/blob/main/extra/banner.png?raw=true "cl-coverage-tools")

`cl-coverage-tools` is a Common Lisp library that provides utilities to build and integrate coverage analysis tools.
It is designed as a foundation for higher-level coverage reporting systems.

## Features

- Collects coverage data during execution.
- Provides processing utilities to associate data with source files.
- Can be used as a backend for custom coverage tools or integrations.

## Usage

1. **Enable coverage collection**:

```lisp
   (cl-coverage-tools:enable-coverage)
```

2. **Run your test suite** as usual. During execution, coverage data will be recorded.

3. **Process the collected coverage data**:

```lisp
   (cl-coverage-tools:process-coverage-data (find-package :my-package) "src/my-file.lisp")
```

   * `:my-package` → the package being tested.
   * `"src/my-file.lisp"` → the file associated with that package.

This will produce structured coverage data that can be used for reporting or further analysis.

## Installation

Clone this repository and load it with [ASDF](https://common-lisp.net/project/asdf/):

```lisp
(asdf:load-system "cl-coverage-tools")
```

Or include it in your project via [Quicklisp](https://www.quicklisp.org/) once it’s available there.

## Reporters available

### CLI Reporter

```lisp
(asdf:load-system "cl-coverage-reporter.cli")

(cl-coverage-reporter.cli:report systems)
```

## License

This project is licensed under the [Unlicense](https://unlicense.org/).
You are free to use, modify, and distribute it without restriction.
