mod token;
mod var_type;

pub type Ident = String;
pub type Token = token::Token<Ident>;

pub use token::Constant;

pub use var_type::VarType;

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
