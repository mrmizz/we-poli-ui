#!/usr/bin/env bash

BUCKET="s3://electionspending.org"

echo "Building New Assets..."
./build.sh
echo "Publishing New Assets..."
aws s3 sync assets/css/ $BUCKET/css/ --profile tap-in
aws s3 cp assets/index.html $BUCKET --profile tap-in
aws s3 cp assets/elm.min.js $BUCKET --profile tap-in
#echo "Invalidating CloudFront Cache..."
#aws cloudfront create-invalidation --distribution-id E2UUWP43BWKWHW --paths "/*" --profile apothera
