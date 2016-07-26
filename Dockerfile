# This dockerfile is not in use, but specifies system installs and dependencies for running Databrary
# Assumes Docker version 1.11.0

FROM centos:7
MAINTAINER "Dylan Simon" <dylan@databrary.org>
ENV container docker
RUN yum -y update
RUN yum -y install epel-release
RUN yum -y install which file gmp-devel git yasm nginx npm cracklib-devel
RUN echo 'fs.protected_hardlinks = 0' > /etc/sysctl.d/hardlinks.conf

WORKDIR /usr/local/src
ENV PKG_CONFIG_PATH=/usr/local/lib/pkgconfig

RUN git clone -b stable git://git.videolan.org/x264.git x264
RUN git clone git://source.ffmpeg.org/ffmpeg.git ffmpeg

ARG lame=3.99.5
RUN curl -L http://sourceforge.net/projects/lame/files/lame/3.99/lame-${lame}.tar.gz | tar -xzf- && \
  cd lame-${lame} && \
  ./configure && make install
ARG fdkaac=0.1.4
RUN curl -L http://sourceforge.net/projects/opencore-amr/files/fdk-aac/fdk-aac-${fdkaac}.tar.gz | tar -xzf- && \
  cd fdk-aac-${fdkaac} && \
  ./configure && make install

ARG ghc=7.10.3b
RUN curl -L https://www.haskell.org/ghc/dist/${ghc%[a-z]}/ghc-${ghc}-x86_64-deb7-linux.tar.xz | tar -xJf- && \
  cd ghc-${ghc} && \
  ./configure && make install

ARG cabal=1.22.9.0
RUN curl -L http://hackage.haskell.org/package/cabal-install-${cabal}/cabal-install-${cabal}.tar.gz | tar -xzf- && \
  cd cabal-install-${cabal} && \
  EXTRA_CONFIGURE_OPTS= ./bootstrap.sh --global

RUN yum -y localinstall http://yum.postgresql.org/9.5/redhat/rhel-7-x86_64/pgdg-centos95-9.5-2.noarch.rpm
RUN yum -y install postgresql95

ARG solr=5.5.0
RUN curl http://mirror.cc.columbia.edu/pub/software/apache/lucene/solr/${solr}/solr-${solr}.tgz | tar -xzf- && \
  ln -s ../src/solr-${solr}/bin/solr /usr/local/bin

ARG ffmpeg=2.8.6
RUN cd x264 && \
  git pull && \
  ./configure --enable-shared && make install
RUN cd ffmpeg && \
  git pull && git checkout n${ffmpeg} && \
  ./configure --enable-shared --enable-gpl --enable-version3 --enable-nonfree --enable-libx264 --enable-libfdk-aac --enable-libmp3lame && make install

RUN yum -y update && yum clean all

RUN useradd -m databrary
USER databrary
WORKDIR /home/databrary
RUN cabal update
RUN cabal install happy
RUN git clone git://github.com/databrary/databrary src

WORKDIR src
ARG commit=origin/stage
RUN git remote update && git checkout $commit
COPY databrary.conf ./
RUN git describe ; ./dev -f -p -i

#CMD ["/bin/bash"]
