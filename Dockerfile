FROM ubuntu

RUN apt-get update -y
RUN apt-get install sudo locales -y

ENV LANG C.UTF-8

RUN sudo apt install smlnj ml-yacc \
    ml-lex ml-burg mlton make \
    ml-ulex rlwrap -y
