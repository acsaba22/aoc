FROM haskell:8

ARG USER_UID
ARG USER_GID
ARG USER_WORKDIR

LABEL maintainer="acsaba@gmail.com"
ENV DOCKERNAME=devhaskell
ENV DEBIAN_FRONTEND=noninteractive
ENV REFRESHED_AT 2020-07-20

RUN apt-get update -q \
        && apt-get upgrade -y -q \
        && apt-get install -y -q --no-install-recommends emacs tmux sudo

################################################################################
## Create the user

ARG guest=dockacs
ARG guest_uid=$USER_UID
ARG guest_gid=$USER_GID

RUN groupadd -g ${guest_gid} ${guest} \
        && useradd --no-log-init -m -s /bin/bash -g ${guest} -G sudo -p '' -u ${guest_uid} ${guest} \
        && mkdir -p -v /home/${guest}/.local/bin \
        && chown -R ${guest}:${guest} /home/${guest} \
        && sed -i -e '/%sudo/s/)/) NOPASSWD:/' /etc/sudoers

USER ${guest}
WORKDIR /home/${guest}

RUN ln -s /home/acsaba/.history .history \
       && ln -s /home/acsaba/r r \
       && rm .bashrc \
       && ln -s /home/acsaba/.bashrc .bashrc \
       && ln -s /home/acsaba/.emacs .emacs \
       && ln -s /home/acsaba/.tmux.conf .tmux.conf

# ADD bashrc.local.sh ./.bashrc.local.sh

# TODO haskell emacs-ot beallitani
# https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
# https://github.com/haskell/haskell-mode/blob/master/README.md

# TODO cabal install regex
# cabal update
# cabal install --lib regex-posix

################################################################################


CMD ["/bin/bash"]
