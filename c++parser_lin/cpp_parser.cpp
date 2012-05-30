#include "cpp_parser.h"

#include <fstream>

#include "creax_threadfifo.h"
#include "creax_thread.h"

#include "Languages/LanComment_Context.h"
#include "Languages/LanAB_Context.h"
#include "Languages/LanCF_Context.h"

int* commentFilterThreadRoutine(LanComment_Context* param)
{
    try {
        LanComment_parse(param);
        param->result = 0;
    } catch (std::runtime_error& err) {
        fprintf(stderr,"comment filter error: %s\n", err.what());
        param->result = -1;
    }
    param->fifo.close_fifo();
    return NULL;
}

int* preprocessorThreadRoutine(LanAB_Context* param)
{
    try {
        LanAB_parse(param);
        param->preprocessor_result = 0;
    } catch (std::runtime_error& err) {
        fprintf(stderr,"preprocessor error: %s\n", err.what());
        param->preprocessor_result = -1;
    }
    param->output_fifo.close_fifo();
    return NULL;
}

/*
int* parserThreadRoutine(LanCD_Context* param)
{
    try {
        LanCD_parse(param);
        param->parser_result = 0;
    } catch (std::runtime_error& err) {
        fprintf(stderr,"parser error: %s\n", err.what());
        param->parser_result = -1;
    }
    return NULL;
}
*/

int* blockParserThreadRoutine(LanCF_Context* param)
{
    try {
        LanCF_parse(param);
        param->parser_result = 0;
    } catch (std::runtime_error& err) {
        fprintf(stderr,"block parser error: %s\n", err.what());
        param->parser_result = -1;
    }
    return NULL;
}

cpp_parser::cpp_parser(const std::string &a_workdir)
    : workdir(a_workdir)
{
}

void cpp_parser::setDefines(std::map<std::string,std::string> a_defines)
{
    defines = a_defines;
}

void cpp_parser::parse_stream(std::istream * const new_input)
{
    creax::threadfifo<text_type> stage1_2;
    creax::threadfifo<code_piece> stage2_3;

#define DEBUG_STAGES

#ifdef DEBUG_STAGES
    creax::threadfifo<text_type> stage1_2a;
    creax::threadfifo<code_piece> stage2_3a;

    // stage 1
    LanComment_Context lanComment_context(*new_input, stage1_2);
    // stage 2
    LanAB_Context lanAB_context(stage1_2a, 1, stage2_3);
    // stage 3
    LanCF_Context lanCFcont(stage2_3a, 1, "", workdir);
#else
    // stage 1
    LanComment_Context lanComment_context(*new_input, stage1_2);
    // stage 2
    LanAB_Context lanAB_context(stage1_2, 1, stage2_3);
    // stage 3
    LanCF_Context lanCFcont(stage2_3, 1, "");
#endif

    //lanAB_context.setCDContext(&lanCDcont);
    lanAB_context.defines.loadDefines(defines);

    lanCFcont.dependencies = new ast_node_define_depencies(NULL);
    lanAB_context.defines.saveDependencies(lanCFcont.dependencies);

//#define USE_THREADS

#ifdef USE_THREADS
    creax::thread<LanComment_Context, int> commentFilterThread(&commentFilterThreadRoutine, &lanComment_context);
    creax::thread<LanAB_Context, int> preprocessorThread(&preprocessorThreadRoutine, &lanAB_context);
    creax::thread<LanCF_Context, int> parserThread(&blockParserThreadRoutine, &lanCFcont);

    commentFilterThread.join();
    preprocessorThread.join();
    parserThread.join();
#else
    commentFilterThreadRoutine(&lanComment_context);

#define PRINT_STAGE_DATA(TEXT)
//#define PRINT_STAGE_DATA(TEXT) TEXT

    PRINT_STAGE_DATA(std::cout << "12 ------------------------------------------------------------------" << std::endl);

    {
        text_type buff;
        while (stage1_2.pop_data(buff))
        {
            PRINT_STAGE_DATA(std::cout << buff.lines << ": " << buff.text << std::endl);
            stage1_2a.push_data(buff);
        }
        stage1_2a.close_fifo();
    }

    preprocessorThreadRoutine(&lanAB_context);

    PRINT_STAGE_DATA(std::cout << "23 ------------------------------------------------------------------" << std::endl);

    {
        code_piece buff;
        while (stage2_3.pop_data(buff))
        {
            PRINT_STAGE_DATA(std::cout << buff.line << ": " << buff.code << std::endl);
            stage2_3a.push_data(buff);
        }
        stage2_3a.close_fifo();
    }

    PRINT_STAGE_DATA(std::cout << "end -----------------------------------------------------------------" << std::endl);

    //parserThreadRoutine(&lanCDcont);
    blockParserThreadRoutine(&lanCFcont);
#endif

    /*std::string text;
    while (context.codefifo.pop_data(text))
    {
        std::cout << text;
    }*/

}

void cpp_parser::parse_file(const std::string &filename)
{
    std::string abs_file;

    if (isAbsolutePath(filename))
    {
        abs_file = filename;
    }
    else
    {
        abs_file = workdir + filename;
    }

    std::ifstream new_input(abs_file.c_str());

    if (!new_input.is_open())
    {
        throw std::runtime_error("could not open file: " + filename);
    }

    parse_stream(&new_input);
}
