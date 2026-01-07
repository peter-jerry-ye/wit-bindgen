use std::panic;

use wit_bindgen_core::abi::{self, AbiVariant, Instruction, LiftLower};
use wit_parser::{Docs, Function, FunctionKind, Resolve};

#[derive(Debug, Default)]
struct TestBindgen {}

impl wit_bindgen_core::abi::Bindgen for TestBindgen {
    type Operand = String;

    fn emit(
        &mut self,
        _resolve: &Resolve,
        inst: &Instruction<'_>,
        _operands: &mut Vec<Self::Operand>,
        results: &mut Vec<Self::Operand>,
    ) {
        println!("{inst:?}");

        // Push dummy results to satisfy stack requirements
        for _ in 0..inst.results_len() {
            results.push("v".to_string());
        }
    }

    fn return_pointer(
        &mut self,
        _size: wit_parser::ArchitectureSize,
        _align: wit_parser::Alignment,
    ) -> Self::Operand {
        unreachable!()
    }

    fn push_block(&mut self) {
        unreachable!()
    }
    fn finish_block(&mut self, _operand: &mut Vec<Self::Operand>) {
        unreachable!()
    }
    fn sizes(&self) -> &wit_parser::SizeAlign {
        unreachable!()
    }
    fn is_list_canonical(&self, _resolve: &Resolve, _element: &wit_parser::Type) -> bool {
        unreachable!()
    }
}

fn simple_func() -> Function {
    Function {
        name: "f".to_string(),
        kind: FunctionKind::Freestanding,
        params: vec![],
        result: None,
        docs: Docs::default(),
        stability: Default::default(),
    }
}

fn simple_async_func() -> Function {
    Function {
        name: "f".to_string(),
        kind: FunctionKind::AsyncFreestanding,
        params: vec![],
        result: None,
        docs: Docs::default(),
        stability: Default::default(),
    }
}

#[test]
fn test_async_import_call() {
    let resolve = Resolve::default();
    let function = simple_func();

    let mut bindgen = TestBindgen::default();
    abi::call(
        &resolve,
        AbiVariant::GuestImportAsync,
        LiftLower::LowerArgsLiftResults,
        &function,
        &mut bindgen,
        true,
    );
}

#[test]
fn test_all_combination() {
    let functions = [simple_func(), simple_async_func()];
    let variants = [AbiVariant::GuestImport, AbiVariant::GuestImportAsync];
    let async_ = [true, false];
    let mut combination = vec![];
    for function in functions.iter() {
        for variants in variants.iter() {
            for async_ in async_.iter() {
                let mut bindgen = TestBindgen::default();
                let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                    abi::call(
                        &Resolve::default(),
                        *variants,
                        LiftLower::LowerArgsLiftResults,
                        function,
                        &mut bindgen,
                        *async_,
                    )
                }));
                if result.is_err() {
                    combination.push((function.name.clone(), *variants, *async_));
                }
            }
        }
    }
    if !combination.is_empty() {
        panic!("The following combinations failed: {combination:?}");
    }
}
