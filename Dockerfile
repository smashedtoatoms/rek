FROM kalilinux/kali-rolling
LABEL maintainer='get@fukt.website'

WORKDIR /home/node/app
ENV TERM=xterm-256color
ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
ENV PATH /home/node/.npm-global/bin:$PATH

# Install some deps and add node user
RUN apt-get -yyq update && apt-get -yyq install sudo curl wget gnupg2 git neovim tmux jq debconf-utils silversearcher-ag-el procps && \
  useradd -ms /bin/bash node && echo "node:node" | chpasswd && adduser node sudo && \
  echo "node ALL=(ALL:ALL) NOPASSWD: ALL" > /etc/sudoers.d/node && chmod 440 /etc/sudoers.d/node

# Install node
RUN curl -sL https://deb.nodesource.com/setup_14.x | sudo -E bash - && \
  apt-get install -yyq nodejs

# Set up mapped node_modules directory for performance perks
RUN mkdir -p /home/node && chown node:node /home/node && \
  mkdir -p /home/node/app/node_modules && chown node:node /home/node/app/node_modules && \
  mkdir -p /home/node/.cache && chown node:node /home/node/.cache && \
  mkdir -p /home/node/.npm-global && chown node:node /home/node/.npm-global && \
  mkdir -p /home/node/.npm && chown node:node /home/node/.npm

# Install Emacs 27 and roswell (comment out section if you don't use emacs and/or common lisp)
RUN mkdir -p /home/node/.emacs.d/vscode-eslint && chown -R node:node /home/node/.emacs.d
COPY ./config/emacs-config/init.el /home/node/.emacs.d/
COPY config/emacs-config/vscode-eslint/ /home/node/.emacs.d/
COPY config/tmux.conf /home/node/.tmux.conf
RUN chown -R node:node /home/node/.emacs.d/* && chown -R node:node /home/node/.tmux.conf
USER node
RUN cd /home/node/.emacs.d/vscode-eslint && npm install && npm run compile:server
USER root
  RUN apt-get -yyq install build-essential automake autoconf libjansson-dev pkg-config libncurses-dev gnutls-dev && \
  cd node_modules && git clone -b emacs-27 --single-branch --depth 1 https://github.com/emacs-mirror/emacs && \
  cd emacs && ./autogen.sh && ./configure --without-makeinfo && make && make install && \
  cd .. && rm -rf emacs && \
  curl -o /home/node/app/node_modules/roswell.deb -sOL `curl -s https://api.github.com/repos/roswell/roswell/releases/latest | jq -r '.assets | .[] | select(.name|test("deb$")) | .browser_download_url'` && \
  dpkg -i /home/node/app/node_modules/roswell.deb && \
  rm -rf /home/node/app/node_modules/roswell.deb
USER node
RUN ros && ros install qlot && ros install sly && emacs --daemon
USER root

# Install tools that I want
RUN DEBIAN_FRONTEND=noninteractive apt-get -yyq install aircrack-ng burpsuite hydra john maltego nmap zaproxy sqlmap wireshark chromium python3-pip \
  telnet && \
  cd node_modules && \
  curl https://raw.githubusercontent.com/rapid7/metasploit-omnibus/master/config/templates/metasploit-framework-wrappers/msfupdate.erb > msfinstall && \
  chmod 755 msfinstall && \
  ./msfinstall && \
  rm -rf msfinstall

# update npm to latest
USER node
RUN npm i npm -g
WORKDIR /home/node/app
