-- DROP Tables (For testing purposes)
DROP TABLE LIBRARIES, ACHIEVEMENTS, USERS, USER_ACHIEVEMENT_RELATIONS, USER_LIBRARY_RELATIONS CASCADE;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE LIBRARIES (
    id              int,
    name            text,
    ip              text,
    points_per_hour int,
    location        text,
    PRIMARY KEY (id)
);

CREATE TABLE USERS (
    id              int,
    client_id       text,
    token           text,
    PRIMARY KEY (id)
);

CREATE TABLE ACHIEVEMENTS (
    id               int,
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
    id               int,
    user_id          int,
    achievement_id   int,
    achievement_time bigint,
    PRIMARY KEY (id),
    FOREIGN KEY (user_id) REFERENCES USERS(id),
    FOREIGN KEY (achievement_id) REFERENCES ACHIEVEMENTS(id)
);

CREATE TABLE USER_LIBRARY_RELATIONS (
    id               int,
    user_id          int,
    library_id       int,
    visit_time       bigint,
    duration         bigint,
    PRIMARY KEY (id),
    FOREIGN KEY (user_id) REFERENCES USERS(id),
    FOREIGN KEY (library_id) REFERENCES LIBRARIES(id)
);

-- Populating the tables
INSERT INTO LIBRARIES VALUES
-- Random values for testing
(0, 'Test Library', '127.0.0.1', 20, '55.947311, -3.201912');

INSERT INTO USERS VALUES
-- Random values for testing
(0, 'Basile Henry', 'token'),
(1, 'Jack Horsburgh', 'token');

INSERT INTO ACHIEVEMENTS VALUES
-- Random values for testing
(0, uuid_generate_v4(), '3D printing', '3D print an object at the Test Library', 50, 0, 1476466113, 1477675680, False),
(1, uuid_generate_v4(), 'Borrowing 1984 by George Orwell', 'Find the book "1984" and read it!', 20, 0, 1476466113, 1477675680, False);
