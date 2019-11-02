all: build-stack-native

## Builds base image
build-base:
	@docker build -t leouno/ubuntu1604-haskell-base -f Dockerfile.base .

build-stack-native: build-base
	@docker build -t leouno/servant-crud .