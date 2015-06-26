DROP TABLE IF EXISTS projectmember;
DROP TABLE IF EXISTS project;
DROP TABLE IF EXISTS member;
DROP TABLE IF EXISTS pi;
DROP TABLE IF EXISTS thrust;


CREATE TABLE thrust (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  name varchar(200)
);

CREATE TABLE pi (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  name varchar(200) UNIQUE,
  thrust uuid REFERENCES thrust(id),
  website varchar(400)
);

CREATE TABLE member(
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  name varchar(200) UNIQUE,
  pi uuid REFERENCES pi(id),
  website varchar(400)
);

CREATE TABLE project (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  name varchar(200) UNIQUE,
  website varchar(400)
);

CREATE TABLE projectmember (
  member uuid REFERENCES member(id),
  project uuid REFERENCES project(id)
);
