resource "aws_route53_zone" "dot-com" {
  name = var.domain_name_dot_com
}

resource "aws_route53_zone" "dot-org" {
  name = var.domain_name_dot_org
}

resource "aws_route53_record" "main-dot-com" {
  zone_id = aws_route53_zone.dot-com.zone_id
  name = aws_route53_zone.dot-com.name
  type = "A"
  alias {
    evaluate_target_health = false
    name = "s3-website-us-west-2.amazonaws.com" //aws_cloudfront_distribution.client_distribution.domain_name
    zone_id = aws_s3_bucket.domain-dot-com.hosted_zone_id //aws_cloudfront_distribution.client_distribution.hosted_zone_id
  }
}

resource "aws_route53_record" "main-dot-org" {
  zone_id = aws_route53_zone.dot-org.zone_id
  name = aws_route53_zone.dot-org.name
  type = "A"
  alias {
    evaluate_target_health = false
    name = "s3-website-us-west-2.amazonaws.com" //aws_cloudfront_distribution.client_distribution.domain_name
    zone_id = aws_s3_bucket.domain-dot-org.hosted_zone_id //aws_cloudfront_distribution.client_distribution.hosted_zone_id
  }
}

resource "aws_route53_record" "www-dot-com" {
  zone_id = aws_route53_zone.dot-com.zone_id
  name = "www.${aws_route53_zone.dot-com.name}"
  type = "A"
  alias {
    evaluate_target_health = false
    name = "s3-website-us-west-2.amazonaws.com" //aws_cloudfront_distribution.client_distribution.domain_name
    zone_id = aws_s3_bucket.sub-domain-www-dot-com.hosted_zone_id //aws_cloudfront_distribution.client_distribution.hosted_zone_id
  }
}

resource "aws_route53_record" "www-dot-org" {
  zone_id = aws_route53_zone.dot-org.zone_id
  name = "www.${aws_route53_zone.dot-org.name}"
  type = "A"
  alias {
    evaluate_target_health = false
    name = "s3-website-us-west-2.amazonaws.com" //aws_cloudfront_distribution.client_distribution.domain_name
    zone_id = aws_s3_bucket.sub-domain-www-dot-org.hosted_zone_id //aws_cloudfront_distribution.client_distribution.hosted_zone_id
  }
}

resource "aws_route53_record" "dev-dot-org" {
  zone_id = aws_route53_zone.dot-org.zone_id
  name = "dev.${aws_route53_zone.dot-org.name}"
  type = "A"
  alias {
    evaluate_target_health = false
    name = "s3-website-us-west-2.amazonaws.com"
    zone_id = aws_s3_bucket.sub-domain-dev-dot-org.hosted_zone_id
  }
}
