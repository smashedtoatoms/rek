FROM kalilinux/kali-rolling
LABEL maintainer='get@fukt.website'

WORKDIR /home/node/app
ENV TERM=xterm-256color
ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
ENV PATH /home/node/.npm-global/bin:/home/node/.roswell/bin:$PATH

# Install some deps and add node user
RUN apt-get -yyq update && apt-get -yyq install sudo curl build-essential automake autoconf pkg-config wget gnupg2 \
    zlib1g-dev git neovim tmux jq debconf-utils silversearcher-ag-el procps && \
  useradd -ms /bin/bash node && echo "node:node" | chpasswd && adduser node sudo && \
  echo "node ALL=(ALL:ALL) NOPASSWD: ALL" > /etc/sudoers.d/node && chmod 440 /etc/sudoers.d/node

# Install node
RUN curl -sL https://deb.nodesource.com/setup_14.x | sudo -E bash - && apt-get install -yyq nodejs

# Set up mapped node_modules directory for performance perks
RUN mkdir -p /home/node && chown node:node /home/node && \
  mkdir -p /home/node/app/node_modules && chown node:node /home/node/app/node_modules && \
  mkdir -p /home/node/.cache && chown node:node /home/node/.cache && \
  mkdir -p /home/node/.npm-global && chown node:node /home/node/.npm-global && \
  mkdir -p /home/node/.npm && chown node:node /home/node/.npm

# Install Roswell (comment out section if you don't use common lisp)
RUN curl -o /home/node/app/node_modules/roswell.deb -sOL \
    `curl -s https://api.github.com/repos/roswell/roswell/releases/latest | jq -r '.assets | .[] | select(.name|test("deb$")) | .browser_download_url'` && \
  dpkg -i /home/node/app/node_modules/roswell.deb && \
  rm -rf /home/node/app/node_modules/roswell.deb
USER node
RUN ros && ros install sbcl && ros use sbcl && ros install qlot && ros install ailisp/linedit && \
  ros install ailisp/prepl && ros install ailisp/cl-lsp
USER root

# Install tools that I want
RUN DEBIAN_FRONTEND=noninteractive apt-get -yyq install netdiscover aircrack-ng burpsuite hydra john maltego nmap \
  zaproxy sqlmap wireshark chromium python3-pip telnet amass net-tools arp-scan inetutils-ping && \
  cd node_modules && \
  curl https://raw.githubusercontent.com/rapid7/metasploit-omnibus/master/config/templates/metasploit-framework-wrappers/msfupdate.erb > msfinstall && \
  chmod 755 msfinstall && \
  ./msfinstall && \
  rm -rf msfinstall

# update npm to latest
USER node
RUN npm i npm -g
WORKDIR /home/node/app
