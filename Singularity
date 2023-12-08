Bootstrap: docker
From: centos:7.9.2009

%labels

    APPLICATION_NAME Miniconda - CentOS 7.9.2009
    APPLICATION_URL https://cw3e.ucsd.edu
    APPLICATION_VERSION 1.0

    AUTHOR_NAME Patrick Mulrooney
    AUTHOR_EMAIL pmulrooney@ucsd.edu

    LAST_UPDATED 2023.06.05

%setup

%environment
    PATH=/opt/conda/bin:$PATH

%files

    ./requirements.txt /
    ./ /opt/StreamflowDashboard

%post -c /bin/bash

    export CONDA_VERSION=py310_22.11.1-1
    export SHA256SUM=00938c3534750a0e4069499baf8f4e6dc1c2e471c86a59caa0dd03f4a9269db6

    echo "=========== "
    yum install -y wget which strace git

    echo "=========== wget conda & verify"
    wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-${CONDA_VERSION}-Linux-x86_64.sh -O miniconda.sh && \
      echo "${SHA256SUM}  miniconda.sh" > miniconda.sha256 && \
      if ! sha256sum --strict -c miniconda.sha256; then exit 1; fi

    echo "=========== install conda"
    mkdir -p /opt && \
      sh miniconda.sh -b -p /opt/conda

    echo "=========== add links"
    ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh && \
      echo ". /opt/conda/etc/profile.d/conda.sh" >> ~/.bashrc && \
      echo "conda activate base" >> ~/.bashrc

    echo "=========== Cleanup "
    rm miniconda.sh miniconda.sha256 && \
      find /opt/conda/ -follow -type f -name '*.a' -delete && \
      find /opt/conda/ -follow -type f -name '*.js.map' -delete && \
      /opt/conda/bin/conda clean -afy

    echo "=========== Init envrionment"
    . /opt/conda/etc/profile.d/conda.sh

    echo "=========== Install from requirements.txt"
    /opt/conda/bin/conda install -y --file /requirements.txt

    #echo "=========== Install dashboard"
    #cd /opt/

    #g#it clone https://github.com/CW3E/StreamflowDashboard.git
    #cp -f /config.yml /opt/StreamflowDashboard/

%runscript

  cd /opt/StreamflowDashboard
  exec R -e "shiny::runApp(host='0.0.0.0', port=8788)"

%test
