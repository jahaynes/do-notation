FROM haskell:8.8.3 AS do-notation-build
RUN apt-get update
RUN apt-get dist-upgrade -y
RUN apt-get install -y libssl-dev
RUN cabal v2-update

WORKDIR /usr/src/haskell

COPY do-notation.cabal .
RUN cabal v2-build --dependencies-only

COPY LICENSE      .
COPY src          ./src
RUN cabal v2-build do-notation

RUN mv dist-newstyle/build/x86_64-linux/ghc-8.8.3/do-notation-0.1.0.0/x/do-notation/build/do-notation/do-notation .
RUN strip do-notation

####################################

FROM debian:stretch

RUN apt-get update               && \
    apt-get dist-upgrade -y      && \
    apt-get install -y libssl1.1    \
                       libgmp10  && \
    apt-get clean                && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /do-notation
COPY                          frontend                     frontend
COPY --from=do-notation-build /usr/src/haskell/do-notation .
CMD ["./do-notation"]
