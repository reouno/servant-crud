FROM fpco/stack-build:lts-13.30 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack install --system-ghc
FROM leouno/ubuntu1604-haskell-base
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libgmp-dev
# NOTICE THIS LINE
COPY --from=build /root/.local/bin .
CMD ["/opt/app/servant-crud-exe"]