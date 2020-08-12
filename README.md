# WTF

This is a Kali docker image with nvim, emacs, node, and common lisp installed.
It operates as the node user in the container because root is scary.  It maps
node_modules to a volume because docker's disk io is terrible, and node beats
the filesystem like the filesystem owes it money.  It uses the node_modules to
stage things because it makes the builds faster.  It also uses docker-compose
because command line docker is hard for my brain.  Finally, it uses a
docker-seccomp.json file so that I can run chrome as non-root with sandboxing in
the container and have it render in x.

# How To Use

1. Install docker and docker-compose.
2. Run `git submodule init && git submodule update` to pull the git submodule
   for the ts lsp server. (You don't need this unless you're going to use emacs,
   like a boss)
3. Build the image from scratch (this takes awhile due to downloads and
   compiles): `cd app && docker-compose up -d --build`
   - If you want to start the already-built instance, use: `cd app && docker-compose up -d`
7. Start bash in the container: `cd app && docker-compose exec rek /bin/bash`
8. ...
9. Profit

# Notes

Anything in the main directory will be available in `/home/node/app` other than
node_modules which is set as a volume for performance reasons.  This allows you
to share files in your host with your image if you want.  The things parallel to
the Dockerfile will persist through restarts, the rest is ephemeral.
