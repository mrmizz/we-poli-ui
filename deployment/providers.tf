terraform {
  backend "s3" {
    bucket = "big-time-tap-in-tf-backends"
    key = "we-poli-ui"
    region = "us-west-2"
  }

  required_providers {
    aws = {
      source = "hashicorp/aws"
      version = "~> 3.40.0"
    }
  }

  required_version = "~>0.14.0"
}


provider "aws" {
  region = "us-west-2"
}

# but I need to make sure that I also have a provider for us-east-1.
# any resources created with this provider will be placed inside
# the us-east-1 zone. This is important for ACM and CloudFront.
provider "aws" {
  region = "us-east-1"
  alias = "us_east_provider"
}
