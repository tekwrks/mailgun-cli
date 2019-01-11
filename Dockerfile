# Builder
FROM haskell:8.4.4 as builder

# Checkout our code onto the Docker container
WORKDIR /app
ADD . /app

# Build and test our code, then install the executable and copy to /app
RUN set -ex; stack setup
RUN set -ex; \
    stack build --no-terminal --copy-bins \
 && cp $(stack path --local-bin)/mailgun-cli .

# Deploy
FROM debian:stretch-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends ca-certificates libgmp10

WORKDIR /
COPY --from=builder /app/mailgun-cli .

ENTRYPOINT ["/mailgun-cli"]
