provider "aws" {
  region = "eu-central-1"
}

# create a new private bucket for Mac
resource "aws_s3_bucket" "mac_bucket" {
  bucket = "hiepph-mac"

  tags = {
    Name = "mac"
    Purpose = "backup"
  }
}

resource "aws_s3_bucket_acl" "mac_bucket_acl" {
  bucket = aws_s3_bucket.mac_bucket.id
  acl    = "private"
}
