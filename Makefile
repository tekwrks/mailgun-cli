projectID=tekwrks
repo=tekwrks
name=mailgun-cli
version=1.0.0

C=stack

.PHONY: build
build:
	$(C) build

.PHONY: install
install:
	$(C) install

.PHONY: test
test:
	$(C) test

.PHONY:image
image:
	docker image build \
		-t ${repo}/${name}:${version} \
		.

