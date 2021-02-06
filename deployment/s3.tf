#######################################################
## DOMAINS ############################################
resource "aws_s3_bucket" "domain-dot-org" {
  bucket = var.domain_name_dot_org
  force_destroy = true
  acl = "public-read"

  website {
    index_document = "index.html"
  }

  tags = {
    Project = var.domain_name_dot_org
    ServiceType = "ui"
  }

  policy = <<POLICY
{
  "Version":"2012-10-17",
  "Statement":[
    {
      "Sid":"AddPerm",
      "Effect":"Allow",
      "Principal": "*",
      "Action":["s3:GetObject"],
      "Resource":["arn:aws:s3:::${var.domain_name_dot_org}/*"]
    }
  ]
}
POLICY
}

resource "aws_s3_bucket" "domain-dot-com" {
  bucket = var.domain_name_dot_com
  force_destroy = true
  website {
    redirect_all_requests_to = var.domain_name_dot_org
  }
}

resource "aws_s3_bucket" "sub-domain-dev-dot-org" {
  bucket = "dev.${var.domain_name_dot_org}"
  force_destroy = true
  acl = "public-read"

  website {
    index_document = "index.html"
  }

  tags = {
    Project = var.domain_name_dot_org
    ServiceType = "ui"
  }

  policy = <<POLICY
{
  "Version":"2012-10-17",
  "Statement":[
    {
      "Sid":"AddPerm",
      "Effect":"Allow",
      "Principal": "*",
      "Action":["s3:GetObject"],
      "Resource":["arn:aws:s3:::dev.${var.domain_name_dot_org}/*"]
    }
  ]
}
POLICY
}

resource "aws_s3_bucket" "sub-domain-www-dot-com" {
  bucket = "www.${var.domain_name_dot_com}"
  force_destroy = true
  website {
    redirect_all_requests_to = var.domain_name_dot_org
  }
}

resource "aws_s3_bucket" "sub-domain-www-dot-org" {
  bucket = "www.${var.domain_name_dot_org}"
  force_destroy = true
  website {
    redirect_all_requests_to = var.domain_name_dot_org
  }
}
#######################################################
