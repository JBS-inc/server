-- DROP Tables (For testing purposes)
DROP TABLE LIBRARIES, ACHIEVEMENTS, USERS, USER_ACHIEVEMENT_RELATIONS, USER_LIBRARY_RELATIONS CASCADE;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE LIBRARIES (
    id              serial,
    name            text,
    ip              text,
    points_per_hour int,
    location        text,
    PRIMARY KEY (id)
);

CREATE TABLE USERS (
    id              serial,
    client_id       text,
    token           text,
    PRIMARY KEY (id)
);

CREATE TABLE ACHIEVEMENTS (
    id               serial,
    achievement_uuid uuid,
    name             text,
    description      text,
    points           int,
    library_id       int,
    creation_time    bigint,
    expiry_time      bigint,
    hidden           boolean,
    PRIMARY KEY (id),
    FOREIGN KEY (library_id) REFERENCES LIBRARIES(id)
);

CREATE TABLE USER_ACHIEVEMENT_RELATIONS (
    id               serial,
    user_id          int,
    achievement_id   int,
    achievement_time bigint,
    PRIMARY KEY (id),
    FOREIGN KEY (user_id) REFERENCES USERS(id),
    FOREIGN KEY (achievement_id) REFERENCES ACHIEVEMENTS(id)
);

CREATE TABLE USER_LIBRARY_RELATIONS (
    id               serial,
    user_id          int,
    library_id       int,
    visit_time       bigint,
    duration         bigint,
    PRIMARY KEY (id),
    FOREIGN KEY (user_id) REFERENCES USERS(id),
    FOREIGN KEY (library_id) REFERENCES LIBRARIES(id)
);

INSERT INTO LIBRARIES VALUES
(0, 'Test Library', '127.0.0.1', 20, '55.947311, -3.201912');

INSERT INTO USERS VALUES
(DEFAULT, 'Basile', 'token'),
(DEFAULT, 'Jack', 'token'),
(DEFAULT, 'Stephan', 'token');

INSERT INTO ACHIEVEMENTS VALUES
-- Example Achievements
(DEFAULT, uuid_generate_v4(), '3D Printing', 'Use the new 3D printers to print an object!', 50, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), '1984', 'Find and read the book "1984" by George Orwell in the library!', 20, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Course: Modeling 3D Objects', 'Attent the library''s course on creating 3D models.', 20, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Event: The "Future of Libraries"', 'Attend the event "The Future of Libraries"', 20, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Welcome', 'Sign up at the library and get access to all its assets!', 20, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Avid reader', 'Borrow your 10th book', 50, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Reading expert', 'Borrow your 50th book', 100, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Spreadsheet Creationist', 'Attend the library''s course on creating spreadsheets', 50, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Cold blooded', 'Borrow your 5th Thriller', 10, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'The Reading begins', 'Borrow your first book', 5, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Loyal Reader', 'Borrow your second book', 5, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Convenient', 'Use one of the library''s computers', 5, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Library celebration', 'Be in the library on your birthday', 100, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Historian', 'Borrow your 5th History novel', 10, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Spooky Library', 'Be in the library on Halloween', 50, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Thank you!', 'Attend our "Save the children" charity event', 40, 0, 1476466113, 1477675680, False),
(DEFAULT, uuid_generate_v4(), 'Not afraid', 'Borrow your 5th Horror novel', 10, 0, 1476466113, 1477675680, False);
