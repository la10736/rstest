use rstest::{fixture, rstest};

use sqlx::PgPool;

struct FixtureStruct {}

#[fixture]
async fn my_fixture() -> FixtureStruct {
    FixtureStruct {}
}

#[rstest]
#[sqlx::test]
async fn test_db(#[future] my_fixture: FixtureStruct, #[ignore] pool: PgPool) {
    assert!(true);
}
