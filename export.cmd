<?xml version="1.0"?>
<CONFIG>
  <CompilerOptions>
    <Version Value="10"/>
    <PathDelim Value="\"/>
    <SearchPaths>
      <IncludeFiles Value="$(ProjOutDir)"/>
    </SearchPaths>
    <Parsing>
      <SyntaxOptions>
        <UseAnsiStrings Value="False"/>
      </SyntaxOptions>
    </Parsing>
    <CodeGeneration>
      <SmallerCode Value="True"/>
      <Optimizations>
        <UncertainOptimizations Value="True"/>
        <OptimizationLevel Value="3"/>
      </Optimizations>
    </CodeGeneration>
    <Linking>
      <Debugging>
        <UseLineInfoUnit Value="False"/>
        <StripSymbols Value="True"/>
        <UseExternalDbgSyms Value="True"/>
      </Debugging>
      <Options>
        <Win32>
          <GraphicApplication Value="True"/>
        </Win32>
      </Options>
    </Linking>
    <Other>
      <Verbosity>
        <ShoLineNum Value="True"/>
      </Verbosity>
      <CompilerMessages>
        <UseMsgFile Value="True"/>
      </CompilerMessages>
      <CompilerPath Value="$(CompPath)"/>
    </Other>
  </CompilerOptions>
</CONFIG>
