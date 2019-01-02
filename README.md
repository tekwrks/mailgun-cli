# mailgun-cli

[![Build Status](https://travis-ci.org/tekwrks/mailgun-cli.svg?branch=master)](https://travis-ci.org/tekwrks/mailgun-cli)

Command line tool to easily send emails through Mailgun.

Accepts mustache variables from: (ascending priority)
  1. configuration YAML file in ```variables``` object
  2. command line arguments as ```key=value```

- persistent configuration file
- mustache templates
- all options accessible through arguments *and* config file for easy automation

