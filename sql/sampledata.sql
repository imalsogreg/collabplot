delete from project;
delete from member;
delete from pi;
delete from thrust;

INSERT INTO thrust(name) VALUES ('Circuits'),('Social'),('Vision'),('Theory'),('Development');
INSERT INTO pi(name,thrust) VALUES
  ('Boyden', (SELECT id FROM thrust WHERE name='Circuits')),
  ('Kanwisher', (SELECT id FROM thrust WHERE name='Social')),
  ('Nakayama', (SELECT id FROM thrust WHERE name='Vision')),
  ('Rosasco', (SELECT id FROM thrust WHERE name='Theory')),
  ('Tennenbaum', (SELECT id FROM thrust WHERE name='Theory')),
  ('Sampolinski', (SELECT id FROM thrust WHERE name='Theory')),
  ('Saxe', (SELECT id FROM thrust WHERE name='Social')),
  ('Spelke', (SELECT id FROM thrust WHERE name='Development')),
  ('Wilson', (SELECT id FROM thrust WHERE name='Circuits')),
  ('Kreiman', (SELECT id FROM thrust WHERE name='Circuits')),
  ('Poggio', (SELECT id FROM thrust WHERE name='Vision')),
  ('Schulz', (SELECT id FROM thrust WHERE name='Development')),
  ('Ullman', (SELECT id FROM thrust WHERE name='Theory')),
  ('Valiant', (SELECT id FROM thrust WHERE name='Theory'))
;

INSERT INTO member(name,pi,website) VALUES
('Hector Penagos', (SELECT id FROM pi WHERE name='Wilson'),Null),
('Ilker Yildirim', (SELECT id FROM pi WHERE name='Tennenbaum'),Null),
('Samuel Gershman', (SELECT id FROM pi WHERE name='Tennenbaum'),Null),
('Sam Norman-Haignere', (SELECT id FROM pi WHERE name='Tennenbaum'),Null)
;

INSERT INTO project(name,website) VALUES
('Topology of hippocampal representations',Null)
;

INSERT INTO projectmember(member,project) VALUES
((SELECT id FROM member WHERE name='Hector Penagos'),
(SELECT id from project WHERE name='Topology  of hippocampal representations')),
((SELECT id FROM member WHERE name='Samuel Gershman'),
(SELECT id from project WHERE name='Topology  of hippocampal representations'));

INSERT INTO mods(userLogin) VALUES ('imalsogreg');
