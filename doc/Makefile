PAGES=$(wildcard _pages/*.md)
BASE_URL=$(shell pwd)
.PHONY: all build

all: build

build:
	bundle exec jekyll build -b $(BASE_URL)/_site

init:
	gem install bundle jekyll
	bundle config set --local path 'vendor/bundle'
	bundle install

_site/index.html: $(PAGES)
	bundle exec jekyll build
