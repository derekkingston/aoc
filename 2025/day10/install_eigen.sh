#!/usr/bin/env bash
EIGEN_VERSION=5.0.0
EIGEN_URL="https://gitlab.com/libeigen/eigen/-/archive/${EIGEN_VERSION}/eigen-${EIGEN_VERSION}.tar.gz"

echo "Downloading Eigen ${EIGEN_VERSION}..."
curl -L "$EIGEN_URL" -o eigen.tar.gz

echo "Removing existing Eigen..."
rm -rf Eigen

echo "Extracting Eigen..."
tar -xzf eigen.tar.gz
mv eigen-${EIGEN_VERSION}/Eigen .

echo "Cleaning up..."
rm -rf eigen-${EIGEN_VERSION}
rm  eigen.tar.gz

echo "Eigen installed!"

