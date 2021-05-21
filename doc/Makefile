PAGES=$(wildcard _pages/*.md)
BASE_URL=$(shell pwd)
.PHONY: all build

all: build

build:
	bundle exec jekyll build -b $(BASE_URL)/_site

watch:
	bundle exec jekyll build -b $(BASE_URL)/_site --watch

init:
	gem install bundle jekyll
	bundle config set --local path 'vendor/bundle'
	bundle install

_site/index.html: $(PAGES)
	bundle exec jekyll build
