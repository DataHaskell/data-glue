FROM fpco/stack-build:lts-11.17

# Install all necessary Ubuntu packages
RUN apt-get update && apt-get install -y python3-pip libgmp-dev libmagic-dev libtinfo-dev libzmq3-dev libcairo2-dev libpango1.0-dev libblas-dev liblapack-dev gcc g++ r-base r-base-core && \
    rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('ggplot2'), repos='http://cran.rstudio.com/')"

RUN curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash - && \
    apt-get install -y nodejs

# Install Jupyter notebook
RUN pip3 install -U jupyterlab
RUN jupyter labextension install ihaskell_jupyterlab

# setup the haskell environment
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
COPY . .

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
CMD ["stack", "exec", "--", "jupyter", "lab", "--ip", "0.0.0.0"]
