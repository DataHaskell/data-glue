FROM fpco/stack-build:lts-11.17

# Install all necessary Ubuntu packages
RUN apt-get update && apt-get install -y python3-pip libgmp-dev libmagic-dev libtinfo-dev libzmq3-dev libcairo2-dev libpango1.0-dev libblas-dev liblapack-dev gcc g++ && \
    rm -rf /var/lib/apt/lists/*

# Install Jupyter notebook
RUN pip3 install -U jupyter

ENV LANG en_US.UTF-8
ENV NB_USER jovyan
ENV NB_UID 1000
ENV HOME /home/${NB_USER}

RUN adduser --disabled-password \
    --gecos "Default user" \
    --uid ${NB_UID} \
    ${NB_USER}

# Set up a working directory for IHaskell
WORKDIR ${HOME}

USER root
RUN chown -R ${NB_UID} ${HOME}
USER ${NB_UID}

# Set up stack
COPY stack.yaml stack.yaml
COPY data-glue.cabal data-glue.cabal

# Install dependencies for IHaskell
USER root
RUN chown -R ${NB_UID} ${HOME}
USER ${NB_UID}

RUN stack setup
RUN stack build && stack install
RUN stack exec -- ihaskell install --stack

USER root
RUN chown -R ${NB_UID} ${HOME}
USER ${NB_UID}

# Run the notebook
CMD ["stack", "exec", "--", "jupyter", "notebook", "--ip", "0.0.0.0"]
