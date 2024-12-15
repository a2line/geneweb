###############################################################################
#                                                      STAGE 1: Build Geneweb
###############################################################################

FROM ocaml/opam:debian-ocaml-4.14-nnp AS builder

ENV OPAMYES=yes
USER root
# Ignore the apt warning here as apt-get does not allow wildcarding versions
# hadolint ignore=DL3027
RUN export DEBIAN_FRONTEND=noninteractive \
    && apt-get update -q \  
    && apt install -yq --no-install-recommends \
        m4 \
        libgmp-dev \
        libpcre3-dev \
        libipc-system-simple-perl=1.* \
        xdot \
        zlib1g-dev \
        perl=5.*\
RUN ln -sf /usr/bin/opam-2.3 /usr/bin/opam
USER opam
RUN git clone https://github.com/geneweb/geneweb.git ./geneweb
WORKDIR /home/opam/geneweb
RUN eval "$(opam env)" && opam install . --deps-only && opam install ancient \
    && ocaml ./configure.ml --sosa-zarith --release && make distrib

###############################################################################
#                                       STAGE 2: Export build via blank image
###############################################################################

FROM scratch AS export
COPY --from=builder /home/opam/geneweb/distribution /

###############################################################################
#                                              STAGE 3: Assemble Docker image
###############################################################################

FROM debian:12-slim AS container

# Install runtime tools and add geneweb user
# Ignore the apt warning here as apt-get does not allow wildcarding versions
# hadolint ignore=DL3027
RUN apt-get update -q \
    && apt install -qy --no-install-recommends sudo openssl \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && adduser --system --group --uid 1000 --home /usr/local/share/geneweb --shell /bin/bash geneweb \
    && usermod -aG sudo geneweb \
    && echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# Add required directories and copy geneweb distribution
USER geneweb
WORKDIR /usr/local/share/geneweb
RUN mkdir -p bin etc log share/data share/dist \
    && echo "172.17.0.1" >> etc/gwsetup_only
COPY --from=builder /home/opam/geneweb/distribution share/dist
COPY geneweb-launch.sh bin/geneweb-launch.sh

EXPOSE 2316-2317
VOLUME [ "/usr/local/share/geneweb/share/data", "/usr/local/share/geneweb/etc" ]
ENV GENEWEB_DATA_PATH=/usr/local/share/geneweb/share/data
ENV GENEWEB_HOME=/usr/local/share/geneweb

CMD [ "bin/geneweb-launch.sh" ]
