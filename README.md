# WTF

This is a Kali docker image with nvim, node, common lisp (via roswell), and some lisp tools installed, along with the
python that kali includes.  It auto-installs tools I find useful. It operates as the node user in the container because
root is scary.  It maps node_modules to a volume because docker's disk io is terrible, and node beats the filesystem
like the filesystem owes it money.  It uses the node_modules to stage things because it makes the builds faster.  It
also uses docker-compose because command line docker is hard for my brain.  Finally, it uses a docker-seccomp.json file
so that I can run chrome as non-root with sandboxing in the container and have it render in x.  I use vscode a lot, so
it's kinda set up to just have vscode hook up to it and be ready to go.

# How To Use

1. Install docker and docker-compose.
2. `touch .env` in the base of the project.  You can use that file to add any environment variables you want.
3. Build the image from scratch (this takes awhile due to downloads and compiles): `cd app && docker-compose up -d
   --build`
   - If you want to start the already-built instance, use: `cd app && docker-compose up -d`
4. Start bash in the container: `cd app && docker-compose exec rek /bin/bash`
5. ...
6. Profit

# Notes

Anything in the main directory will be available in `/home/node/app` other than node_modules which is set as a docker
volume for performance reasons.  It will only be accessible within your docker container.  This allows you to share
files in your host with your image if you want.  The things parallel to the Dockerfile will persist through restarts,
the rest of the container's contents are ephemeral, and will be gone every time you restart the container.

# Personal Notes
- Find arp entries for everything on my wifi: `sudo arp-scan -I wlp0s20f3 -l`
- Scan an ip for ports to beat the shit out of (T is speed, p is port, A is detail): `nmap -T4 -p - -A 192.168.0.66`
